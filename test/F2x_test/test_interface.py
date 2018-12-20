from F2x_test.interface.lib import sub_call_glue as sub_call


def test_subcall_input_args():
    sub_call.INPUT_ARGS(1, 2.3, True)
    sub_call.INPUT_ARGS(0, 0.0, False)

def test_subcall_return_integer():
    assert sub_call.RETURN_INTEGER() == 42
