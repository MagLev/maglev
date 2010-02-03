# Processes the output file from a test_irb.sh run
# Printing every test that has a: Maybe IRB bug!!

/^==> Running/ {
    test_name = $NF
    result_printed_yet = "false"
}


/Maybe IRB bug/ {
    if ( result_printed_yet == "false" ) {
        print test_name " failed!"
        result_printed_yet = "true"
    }
}

