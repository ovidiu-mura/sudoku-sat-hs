#!/bin/sh
PGM="`basename $0`"
USAGE="$PGM: usage: $PGM [--minisat2|--picosat] [<problem>]"
PROBTMP=/tmp/prob.$$
SOLNTMP=/tmp/soln.$$
#trap "rm -f $PROBTMP $SOLNTMP" 0 1 2 3 15
SOLVER=picosat
case $1 in
    --minisat2) SOLVER="minisat2" ; shift ;;
esac
case $# in
    0) runghc ./sudoku-encode.hs >$PROBTMP ;;
    1) runghc ./sudoku-encode.hs <"$1" >$PROBTMP ;;
    *) echo "$USAGE" >&2 ; exit 1 ;;
esac
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
