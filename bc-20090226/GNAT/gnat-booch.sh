if [[ (${USER} != root) && (-z ${BC_Path} ) ]] ; then
    declare -x BC_Path=/opt/gnat/include
    declare -x ADA_INCLUDE_PATH=${ADA_INCLUDE_PATH}:${BC_Path}
    declare -x ADA_PROJECT_PATH=${ADA_PROJECT_PATH}:${BC_Path}/GNAT
    declare -x ADA_OBJECTS_PATH=${ADA_OBJECTS_PATH}:/opt/gnat/lib/booch
fi;

# vim: textwidth=0 nowrap tabstop=8 shiftwidth=4 softtabstop=4 expandtab
# vim: filetype=sh encoding=utf-8 fileformat=unix
