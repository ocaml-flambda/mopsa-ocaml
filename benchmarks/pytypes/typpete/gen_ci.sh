FILES=$(grep -x -l "import mopsa" *.py)
MAX=$(echo $FILES | wc -w)

i=1
for f in $FILES; do
    echo "bench-typpete $i/$MAX:"
    echo "  variables:"
    echo "    PY_FILE: '$f'"
    echo "  extends: .bench-typpete"
    echo ""
    i=$((i+1))
done
