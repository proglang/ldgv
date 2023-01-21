clear; echo "Add"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < dev-examples/add/server.ldgvnw & stack run ldgv -- interpret < dev-examples/add/client.ldgvnw & wait);
exit;