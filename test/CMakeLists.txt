
add_executable(test_nanorange
    catch_main.cpp
    test_non_modifying_seq_ops.cpp
    test_modifying_seq_ops.cpp
    test_partitioning_ops.cpp
    test_sorting_ops.cpp
    test_type_traits.cpp
)
target_compile_definitions(test_nanorange PRIVATE "-DTCB_RANGES_NO_DEPRECATION_WARNINGS")

