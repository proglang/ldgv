for i in {1..100}; do
    clear; echo "Add"; stack run ldgv -- interpret < ../networking-examples/add/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/add/client.ldgvnw;
    sleep 0.5;
    clear; echo "Simple"; stack run ldgv -- interpret < ../networking-examples/simple/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/simple/client.ldgvnw;
    sleep 0.5;
    clear; echo "Bidirectional"; stack run ldgv -- interpret < ../networking-examples/bidirectional/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/bidirectional/client.ldgvnw;
    sleep 0.5;
    clear; echo "Handoff"; stack run ldgv -- interpret < ../networking-examples/handoff/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff/handoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff/client.ldgvnw;
    sleep 0.5;
    # clear; echo "Handoff2"; stack run ldgv -- interpret < ../networking-examples/handoff2/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff2/handoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff2/client.ldgvnw; 
    # sleep 0.5;
    # clear; echo "Bidirhandoff"; stack run ldgv -- interpret < ../networking-examples/bidirhandoff/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/bidirhandoff/handoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/bidirhandoff/handoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/bidirhandoff/client.ldgvnw; 
    # sleep 0.5;
done