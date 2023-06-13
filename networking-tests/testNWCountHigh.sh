for i in {1..20000}; do
    clear; echo "$i Add"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/add/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/add/client.ldgvnw & wait);
    clear; echo "$i SendString"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/sendString/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/sendString/client.ldgvnw & wait);
    clear; echo "$i TwoCons"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/twoCons/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/twoCons/client.ldgvnw & wait);
    clear; echo "$i Add2"; stack run ldgv -- interpret ../networking-examples/add2/add.ldgvnw
    clear; echo "$i Simple"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/simple/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/simple/client.ldgvnw & wait);
    clear; echo "$i Bidirectional"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/bidirectional/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/bidirectional/client.ldgvnw & wait);
    clear; echo "$i Handoff"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/handoff/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff/handoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff/client.ldgvnw & wait);
    clear; echo "$i Handoff2"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/handoff2/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff2/handoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff2/client.ldgvnw & wait);
    clear; echo "$i Handoff3"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/handoff3/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff3/handoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff3/client.ldgvnw & wait); 
    clear; echo "$i Handoff4"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/handoff4/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff4/handoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff4/client.ldgvnw & wait); 
    clear; echo "$i Handoff5"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/handoff5/add.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff5/handoff.ldgvnw & wait);  
    clear; echo "$i Handoff6"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/handoff6/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff6/handoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff6/client.ldgvnw & wait); 
    clear; echo "$i Handoff7"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/handoff7/add.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff7/handoff.ldgvnw & wait);  
    clear; echo "$i Handoff8"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/handoff8/add.ldgvnw & stack run ldgv -- interpret < ../networking-examples/handoff8/handoff.ldgvnw & wait);     
    clear; echo "$i Bidirhandoff"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/bidirhandoff/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/bidirhandoff/serverhandoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/bidirhandoff/clienthandoff.ldgvnw & stack run ldgv -- interpret < ../networking-examples/bidirhandoff/client.ldgvnw & wait); 
done