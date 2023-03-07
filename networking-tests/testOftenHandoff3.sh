for i in {1..200000}; do
    clear; echo "$i Handoff3"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/handoff3/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff3/handoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff3/client.ldgvnw & wait); 
done