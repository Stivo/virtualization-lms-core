set -e
set -x
rm *.png || true
rm *.dot || true
mv ../test*.dot .
for x in test*.dot
do
dot -Tpng -o $x.png $x
done
#cp -u test.png show.png

