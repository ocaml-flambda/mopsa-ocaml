# /!\ variables FORCE_ASSERT_UNREACH FILE CONF1 REGEX1 CONF2 REGEX2 need to be exported!

clang -c $FILE &> /dev/null &&
    ( ( [ $FORCE_ASSERT_UNREACH -eq 0 ] && grep -q "_mopsa_assert_unreachable();" $FILE) || true )  &&
    ( mopsa-c -no-color -config=c/$CONF1 $FILE 2>&1 > stdout1 || true ) &&
    grep -q "$REGEX1" stdout1 &&
    ( mopsa-c -no-color -config=c/$CONF2 $FILE 2>&1 > stdout2 || true ) &&
    grep -q "$REGEX2" stdout2
