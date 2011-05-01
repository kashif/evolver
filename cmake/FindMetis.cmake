FIND_PATH(METIS_INCLUDE_DIR metis.h
          /opt/local/include
          /usr/local/include
          /usr/include
          /usr/include/metis)

FIND_LIBRARY(METIS_LIBRARY metis
            /opt/local/lib
            /usr/local/lib
            /usr/lib)
  
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LibMetis  DEFAULT_MSG
                                  METIS_LIBRARY METIS_INCLUDE_DIR)

MARK_AS_ADVANCED( METIS_INCLUDE_DIR METIS_LIBRARY )