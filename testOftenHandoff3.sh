for i in {1..20000}; do
    clear; echo "$i Handoff3"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < dev-examples/handoff3/server.ldgvnw & stack run ldgv -- interpret < dev-examples/handoff3/handoff.ldgvnw & stack run ldgv -- interpret < dev-examples/handoff3/client.ldgvnw & wait); 
done