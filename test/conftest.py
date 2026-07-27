"""Check that no test leaves an error behind on the isl context.

The binding tells "isl reported why this failed" apart from "isl returned
nothing and had nothing to say" by looking at the context right after
a call.  That only works while the context is clean to begin with, which
in turn requires every call that produces an error to consume it.
This fixture is what keeps that honest.
"""
import pytest
import isl


@pytest.fixture(autouse=True)
def isl_context_left_clean():
  ctx = isl.Context.getDefaultInstance()
  isl.isl.isl_ctx_reset_error(ctx)
  yield
  msg = isl.isl.isl_ctx_last_error_msg(ctx)
  assert msg is None, (
    "test left an unconsumed isl error behind: %s" % msg.decode('ascii'))
