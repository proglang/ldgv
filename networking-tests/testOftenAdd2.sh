for i in {1..2000}; do
    clear; echo "$i Add2"; stack run ldgv -- interpret ../networking-examples/add2/add.ldgvnw
done