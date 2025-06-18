#!/bin/bash

# Help Message
usage () {
    cat <<EOF

Usage: $0 [OPTIONS] [TRACE_FOLDER]

Options:
  -h, --help                       # Shows this message
  -u, --ukilli TRACE_FOLDER        # Execute only ukilli on TRACE_FOLDER
  -1, --svztikki1 TRACE_FOLDER     # Execute only svztikki step 1 on TRACE_FOLDER
  -2, --svztikki2 TRACE_FOLDER     # Execute only svztikki step 2 on TRACE_FOLDER
  -a, --application APPLICATION    # Hard define APPLICATION for scripts
  -c, --config CONFIG_FILE         # Use CONFIG_FILE as config file
  -e, --exclude-tasks TASKLIST     # Exclude tasks from the analysis
EOF
}

EXEC_ALL=1
EXEC_UKILLI=0
EXEC_STEP_1=0
EXEC_STEP_2=0
TRACE_FOLDER="./"
CONFIG_FILE=""

while true; do
        case "$1" in
                --ukilli|-u )
                        EXEC_UKILLI=1
			      EXEC_STEP_1=0
                        EXEC_STEP_2=0
                        EXEC_ALL=0
			      shift 1
			      ;;
                --svztikki1|-1 )
                        EXEC_UKILLI=0
			      EXEC_STEP_1=1
                        EXEC_STEP_2=0
                        EXEC_ALL=0
                        shift 1
                        ;;
                --svztikki2|-2 )
                        EXEC_UKILLI=0
			      EXEC_STEP_1=0
                        EXEC_STEP_2=1
                        EXEC_ALL=0
                        shift 1
                        ;;
                --help|-h)
                        usage;
                        exit 0;
                        ;;
                --config|-c)
                        CONFIG_FILE=$(readlink -f $2)
                        shift 2
                        ;;
                --exclude-tasks|-e)
                        export EXCLUDE_TASKS=$2
                        shift 2
                        ;;
                "")
                        break
                        ;;
		      *)
                        # Make it absolute
                        TRACE_FOLDER=$(cd $1; pwd)
                        shift 1
                        ;;
        esac
done

  if [ ! -x "$(command -v ukilli)" ]; then
      echo "ERROR: Required application ukilli not found."
      echo "You can install it with: <spack install tikki> and then spack load tikki>"
      exit 1
  elif [ ! -x "$(command -v recsel)" ]; then
      echo "ERROR: Required application recsel/rec2csv not found."
      echo "Please, install the <recutils> pakage at your system."
      exit 1
  fi

  if [ "$EXEC_ALL" -eq 1 ] || [ "$EXEC_UKILLI" -eq 1 ]; then
      olddir=$PWD
      cd $TRACE_FOLDER
      ukilli --csv trace-*
      ukilli --somp trace-*
      ukilli --dot trace-*
      ukilli --paje trace-*
      ukilli --rastello trace-*
      cat trace_*rec | recsel -p Name,JobId,DependsOn | rec2csv > dag-do-rec-raw.csv
      if [ -n "$EXCLUDE_TASKS" ]; then
	      greprm="grep "
	      for i in $(echo $EXCLUDE_TASKS | sed "s/,/ /g")
	      do
		  for rec in `ls -1 *.rec`;
		  do
		      echo "working on $rec"
		      if [ -s "$rec" ]; then
			  recdel -e "Name = '$i'" $rec
		      fi
		  done
            greprm="$greprm -v $i"
	      done
	      mv tasks.csv tasks0.csv
	      greprm="$greprm tasks0.csv > tasks.csv"
	      eval $greprm
      fi
      cd $olddir
  fi

  if [ "$EXEC_ALL" -eq 1 ] || [ "$EXEC_STEP_1" -eq 1 ]; then
      Rscript starvz-tikki-phase1.R $TRACE_FOLDER
  fi

  if [ "$EXEC_ALL" -eq 1 ] || [ "$EXEC_STEP_2" -eq 1 ]; then
      Rscript starvz-tikki-phase2.R $TRACE_FOLDER
  fi
