MOPSAROOT=$(git rev-parse --show-toplevel)
FILES=$(grep -x -l "import mopsa" $MOPSAROOT/benchmarks/pytypes/typpete/*.py)
MAX=$(echo $FILES | wc -w)

sed -n "/bench-typpete /q;p" $MOPSAROOT/.gitlab-ci.yml
i=1
for f in $FILES; do
    echo "bench-typpete $i/$MAX:"
    echo "  variables:"
    echo "    PY_FILE: '$(basename $f)'"
    echo "  extends: .bench-typpete"
    echo ""
    i=$((i+1))
done
