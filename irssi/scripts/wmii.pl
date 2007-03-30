# Ganked straight outta hilightwin.pl. It's public domain.
use Irssi;
use vars qw($VERSION %IRSSI);
$VERSION = "0.01";
%IRSSI = (
	authors => 'pd',
	name => 'irssi wmii notification',
	description => 'in conjunction with an rc script, writes irssi hilights to the wmii bar',
	license => 'WTFPL'
);

sub sig_printtext {
	my ($dest, $text, $stripped) = @_;
	if (($dest->{level} & (MSGLEVEL_HILIGHT|MSGLEVEL_MSGS)) && ($dest->{level} & MSGLEVEL_NOHILIGHT) == 0) {
		if ($dest->{level} & MSGLEVEL_PUBLIC) {
			$stripped = $dest->{target}.": ".$stripped;
		}
		`rc $ENV{HOME}/.wmii-3.5/hiirc.rc "$stripped"`
	}
}

Irssi::signal_add('print text', 'sig_printtext');
