################
# Additional xpce tests (plunit-based, standalone modules)

test_libs(number_conversion)
test_libs(object_reference)

set_tests_properties(xpce:number_conversion xpce:object_reference
                     PROPERTIES ENVIRONMENT "${XPCE_TEST_ENV}")
