for i in {1..2000}; do
    clear; echo "$i Handoff8"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/handoff8/add.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff8/handoff.ldgvnw & wait); 
done