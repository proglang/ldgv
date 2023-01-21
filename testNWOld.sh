for i in {1..100}; do
    clear; echo "Add"; stack run ldgv -- interpret < dev-examples/add/server.ldgvnw & stack run ldgv -- interpret < dev-examples/add/client.ldgvnw;
    sleep 0.5;
    clear; echo "Simple"; stack run ldgv -- interpret < dev-examples/simple/server.ldgvnw & stack run ldgv -- interpret < dev-examples/simple/client.ldgvnw;
    sleep 0.5;
    clear; echo "Bidirectional"; stack run ldgv -- interpret < dev-examples/bidirectional/server.ldgvnw & stack run ldgv -- interpret < dev-examples/bidirectional/client.ldgvnw;
    sleep 0.5;
    clear; echo "Handoff"; stack run ldgv -- interpret < dev-examples/handoff/server.ldgvnw & stack run ldgv -- interpret < dev-examples/handoff/handoff.ldgvnw & stack run ldgv -- interpret < dev-examples/handoff/client.ldgvnw;
    sleep 0.5;
    # clear; echo "Handoff2"; stack run ldgv -- interpret < dev-examples/handoff2/server.ldgvnw & stack run ldgv -- interpret < dev-examples/handoff2/handoff.ldgvnw & stack run ldgv -- interpret < dev-examples/handoff2/client.ldgvnw; 
    # sleep 0.5;
    # clear; echo "Bidirhandoff"; stack run ldgv -- interpret < dev-examples/bidirhandoff/server.ldgvnw & stack run ldgv -- interpret < dev-examples/bidirhandoff/handoff.ldgvnw & stack run ldgv -- interpret < dev-examples/bidirhandoff/handoff.ldgvnw & stack run ldgv -- interpret < dev-examples/bidirhandoff/client.ldgvnw; 
    # sleep 0.5;
done