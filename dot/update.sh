set -e

dot -Tpng -o test.png ../test.dot

cp -u test.png show.png
