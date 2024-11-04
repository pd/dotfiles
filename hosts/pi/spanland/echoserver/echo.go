package main

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"net"
	"os"
	"strings"
	"time"
)

const listenAddr = "0.0.0.0:1025"

// Silly TCP+UDP echo server used during networking talks
func main() {
	if err := mainErr(context.Background()); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func mainErr(ctx context.Context) error {
	tcp, err := net.Listen("tcp", listenAddr)
	if err != nil {
		return fmt.Errorf("Listen %s: %w", listenAddr, err)
	}
	defer tcp.Close()

	udpAddr, err := net.ResolveUDPAddr("udp", listenAddr)
	if err != nil {
		return fmt.Errorf("ResolveUDPAddr: %w", err)
	}

	udp, err := net.ListenUDP("udp", udpAddr)
	if err != nil {
		return fmt.Errorf("ListenUDP %s: %w", listenAddr, err)
	}
	defer udp.Close()

	log("TCP: listening on %s", listenAddr)
	log("UDP: listening on %s", listenAddr)

	go serveUDP(ctx, udp)
	go serveTCP(ctx, tcp)

	<-ctx.Done()
	return nil
}

func serveTCP(ctx context.Context, tcp net.Listener) {
	for {
		conn, err := tcp.Accept()
		if err != nil {
			log("TCP: Accept error: %s", err)
			continue
		}

		go func(conn net.Conn) {
			defer conn.Close()
			buf := bufio.NewReader(conn)

			local := conn.LocalAddr()
			remote := conn.RemoteAddr()
			log("TCP: %s <- %s: connection accepted", local, remote)

			for {
				msg, err := buf.ReadBytes(byte('\n'))
				if err != nil {
					if err == io.EOF {
						log("TCP: %s <- %s: connection closed", local, remote)
					} else {
						log("TCP: %s <- %s: read error: %s", local, remote, err)
					}
					return
				}

				log("TCP: %s <- %s: %s", local, remote, msg)
				_, err = conn.Write(msg)
				if err != nil {
					log("TCP: %s -> %s: error writing: %s", local, remote, msg)
					return
				}
				log("TCP: %s -> %s: %s", local, remote, msg)
			}

		}(conn)
	}
}

func serveUDP(ctx context.Context, udp *net.UDPConn) {
	buf := make([]byte, 256)
	localAddr := udp.LocalAddr()

	for {
		select {
		case <-ctx.Done():
			return
		default:
		}

		n, addr, err := udp.ReadFromUDP(buf)
		if err != nil {
			log("UDP: ReadFromUDP error: %s", err)
			continue
		}

		msg := buf[:n]
		log("UDP: %s <- %s: %s", localAddr, addr, msg)

		_, err = udp.WriteToUDP(msg, addr)
		if err != nil {
			log("UDP: WriteToUDP(%s): %s", addr, err)
			continue
		}
		log("UDP: %s -> %s: %s", localAddr, addr, msg)
	}
}

func log(msg string, args ...any) {
	msg = fmt.Sprintf(msg, args...)
	tm := time.Now().Format(time.RFC3339)
	if strings.HasSuffix(msg, "\n") {
		fmt.Printf("%s %s", tm, msg)
	} else {
		fmt.Printf("%s %s\n", tm, msg)
	}
}
