for i in {1..2000}; do
    clear; echo "$i Handoff5"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/handoff5/add.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff5/handoff.ldgvnw & wait); 
done