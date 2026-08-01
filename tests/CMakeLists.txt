################
# Additional xpce tests (plunit-based, standalone modules)

test_libs(number_conversion)

set_tests_properties(xpce:number_conversion
                     PROPERTIES ENVIRONMENT "${XPCE_TEST_ENV}")
