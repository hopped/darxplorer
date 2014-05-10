#! /bin/bash

# Sources
DIRECTORY=..
MAIN_SOURCES=$DIRECTORY/src
COMPILER_SOURCES=$MAIN_SOURCES/compiler
PARSER_SOURCES=$COMPILER_SOURCES/parser
SUPPORT_SOURCES=$PARSER_SOURCES/support
LEXER_SOURCES=$COMPILER_SOURCES/lexer
FUNCTION_SOURCES=$MAIN_SOURCES/function
ADA_BOOCH=$DIRECTORY/bc-20090226
BC_SOURCES=$ADA_BOOCH/src
BC_DEMOS=$ADA_BOOCH/demos

# Output
OBJDIR="obj"

# GNATMAKE
GNATMAKE="$(which gnatmake)"
## gnatmake not found in path, use old default
if [ "$GNATMAKE" = "" ]
  then GNATMAKE="/usr/local/ada-4.3/bin/gnatmake"
fi

# Compiler flags
CORES_TIMES_TWO=4
GNATFLAGS="-O3 -m -p -gnatVa -gnato -fstack-check -gnata -gnatyx -gnat05 -j$CORES_TIMES_TWO -gnatwcl.ormu"

# LOG
LOG_FILE="../darxplorer.log"

Compile_Framework()
{
  cd $OBJDIR
  $GNATMAKE -I$MAIN_SOURCES -I$BC_SOURCES -I$BC_DEMOS -I$COMPILER_SOURCES -I$PARSER_SOURCES -I$SUPPORT_SOURCES -I$LEXER_SOURCES $GNATFLAGS $COMPILER_SOURCES/$2.adb &> $LOG_FILE
  output="$(tail -1 $LOG_FILE)"
  output="$(echo $output | tr ' ' '\n' | grep -n "error")"
  if [ "$output" != "" ]
    then return 2 # ERROR
  fi
  mv $2 ../$3
  cd ..
  output="$(./$3 $1)"
  output="$(echo $output | tr ' ' '\n' | grep -n "TRUE")"
  if [ "$output" != "" ]
    then return 1 # TRUE
    else return 0 # FALSE
  fi
}

Compile_Function()
{
  cd $OBJDIR
  $GNATMAKE -I$MAIN_SOURCES -I$FUNCTION_SOURCES -I$BC_SOURCES -I$BC_DEMOS $GNATFLAGS dxpl-darxplorer.adb &> $LOG_FILE
  output="$(tail -1 $LOG_FILE)"
  output="$(echo $output | tr ' ' '\n' | grep -n "error")"
  if [ "$output" != "" ]
    then return 2
  fi
  mv dxpl-darxplorer ../darxplorer
  cd ..
  output="$(./darxplorer)"
  output="$(echo $output | tr ' ' '\n' | grep -n "TRUE")"
  if [ "$output" != "" ]
    then return 0 # TRUE
    else return 1 # FALSE
  fi
}

Run()
{
  rm -r $OBJDIR &> output
  mkdir $OBJDIR &> output
  rm output

  echo -e "++ The framework will be compiled .. \c"
  DXPL_COMPILE="darxplorer-compile"
  Compile_Framework $1 "dxpl-compiler-cli" $DXPL_COMPILE
  case $? in
    1)
    echo "FINISHED!"
    ;;
    0)
    echo
    echo "-- Your description could not be parsed due to some errors."
    echo "-- Please correct your description and try again."
    return 1
    ;;
    2)
    echo "ERROR!"
    echo "-- An error occured while compiling. Please see 'darxplorer.log' for more information"
    return 1
    ;;
  esac

  echo -e "++ darXplorer will be compiled by means of your description .. \c"
  Compile_Function
  case $? in
    1)
    echo "FINISHED!"
    echo
    echo "   DARXplorer has validated your description."
    echo "   The application 'darxplorer' is ready to run."
    ;;
    0)
    echo "ERROR!"
    echo "-- Your description wasn't valid."
    echo "-- Please correct your description and try again."
    return 1
    ;;
    2)
    echo "ERROR!"
    echo "-- An error occured while compiling. Please see 'darxplorer.log' for more information"
    return 1
    ;;
  esac

  # clean up
  find . -name 'b~*' -delete
  rm $DXPL_COMPILE
  return 0
}

Main ()
{
  if [ $# -eq 0 ]
    then 
      echo "-- Please provide a description of a hash function."
      return 1
    else
      Run $1
  fi
  return 0
}

Main $1
