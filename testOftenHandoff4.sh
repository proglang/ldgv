for i in {1..2000}; do
    clear; echo "$i Handoff4"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < networking-examples/handoff4/server.ldgvnw & stack run ldgv -- interpret < networking-examples/handoff4/handoff.ldgvnw & stack run ldgv -- interpret < networking-examples/handoff4/client.ldgvnw & wait); 
done