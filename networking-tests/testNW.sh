for i in {1..100}; do
    bash testAdd.sh;
    bash testSimple.sh;
    bash testBidirectional.sh;
    bash testHandoff.sh;
    bash testHandoff2.sh;
    bash testBidirhandoff.sh;
done