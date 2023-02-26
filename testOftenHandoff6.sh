for i in {1..2000}; do
    clear; echo "$i Handoff6"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < dev-examples/handoff6/server.ldgvnw & stack run ldgv -- interpret < dev-examples/handoff6/handoff.ldgvnw & stack run ldgv -- interpret < dev-examples/handoff6/client.ldgvnw & wait); 
done