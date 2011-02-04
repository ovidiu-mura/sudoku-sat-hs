#!/bin/sh
# Copyright © 2011 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

# Driver for SAT-solver-based Sudoku solver.

# Set some things up.
PGM="`basename $0`"
USAGE="$PGM: usage: $PGM [--minisat2|--picosat] [<problem>]"
PROBTMP=/tmp/prob.$$
SOLNTMP=/tmp/soln.$$
trap "rm -f $PROBTMP $SOLNTMP" 0 1 2 3 15

# Select a solver.
SOLVER=picosat
case $1 in
    --minisat2) SOLVER="minisat2" ; shift ;;
esac

# Read from a supplied file, else from standard input.
case $# in
    0) runghc ./sudoku-encode.hs >$PROBTMP ;;
    1) runghc ./sudoku-encode.hs <"$1" >$PROBTMP ;;
    *) echo "$USAGE" >&2 ; exit 1 ;;
esac

# Finally, invoke a solver to solve the SAT instance.
case $SOLVER in
    picosat)
	picosat $PROBTMP >$SOLNTMP
	;;
    minisat2)
	minisat2 $PROBTMP $SOLNTMP >/dev/null
	;;
    *)
	echo "$PGM: unknown solver" >&2
	exit 1
	;;
esac
# Could the solver find an assignment? These return codes are
# apparently standard. We will use them too.
case $? in
    10)
	;;
    20)
	echo "problem has no legal solution" >&2
	exit 20
	;;
    *)
	echo "unexpected $SOLVER exit code $?"
	exit 1
	;;
esac
# Clean up the garbage text in the assignment, and
# pass it to the decoder for display.
case $SOLVER in
    picosat)
	sed -e '/^v /!d' -e 's/^v //' <$SOLNTMP
	;;
    minisat2)
	sed -e '/^SAT/d' <$SOLNTMP
	;;
    *)
	echo "$PGM: unknown solver" >&2
	exit 1
	;;
esac |
runghc ./sudoku-decode.hs
exit 10
