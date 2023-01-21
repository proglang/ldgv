clear; echo "Simple"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < dev-examples/simple/server.ldgvnw & stack run ldgv -- interpret < dev-examples/simple/client.ldgvnw & wait);
exit;