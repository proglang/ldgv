clear; echo "Handoff"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/handoff/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff/handoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff/client.ldgvnw & wait);
exit;