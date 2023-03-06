clear; echo "Recursion"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < dev-examples/recursion/server.ldgvnw & stack run ldgv -- interpret < dev-examples/recursion/client.ldgvnw & wait);
exit;