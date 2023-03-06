clear; echo "Add"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < networking-examples/add/server.ldgvnw & stack run ldgv -- interpret < networking-examples/add/client.ldgvnw & wait);
exit;