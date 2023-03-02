for i in {1..2000}; do
    clear; echo "$i Handoff7"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < dev-examples/handoff7/add.ldgvnw & stack run ldgv -- interpret < dev-examples/handoff7/handoff.ldgvnw & wait); 
done