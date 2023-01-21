for i in {1..2000}; do
    clear; echo "$i Bidirhandoff"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < dev-examples/bidirhandoff/server.ldgvnw & stack run ldgv -- interpret < dev-examples/bidirhandoff/serverhandoff.ldgvnw & stack run ldgv -- interpret < dev-examples/bidirhandoff/clienthandoff.ldgvnw & stack run ldgv -- interpret < dev-examples/bidirhandoff/client.ldgvnw & wait); 
done