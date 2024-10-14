personal infra automation

controls:

1. the NUC media center running arch+kodi
2. the DO znc/webhost/etc

needs env:

```
DIGITALOCEAN_TOKEN
B2_APPLICATION_KEY_ID
B2_APPLICATION_KEY
AWS_ACCESS_KEY_ID="$B2_APPLICATION_KEY_ID"
AWS_SECRET_ACCESS_KEY="$B2_APPLICATION_KEY"
```
