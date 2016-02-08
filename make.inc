
# Set directories

#EXTERNAL_LIBRARY_PATH = /usr/local/lib
EXTERNAL_LIBRARY_PATH = /usr/local/lib64

BIN_PATH = /usr/local/bin

SRC_DIRECTORY = ./

SOURCES = $(wildcard $(SRC_DIRECTORY)/*.f90)

OBJECTS := $(patsubst %.f90, %.o, $(wildcard *.f90))

OBJ_DIRECTORY = ../objs

LIB_DIRECTORY = ../lib

OBJECTS_WITH_PREFIX = $(addprefix $(OBJ_DIRECTORY)/, $(OBJECTS))

# Set compiler options

FC = gfortran

FFLAGS = \
$(LANGUAGE_OPTIONS) \
$(ERROR_WARNING_OPTIONS) \
$(CODE_GENERATION_OPTIONS)

LANGUAGE_OPTIONS = \
-fimplicit-none

ERROR_WARNING_OPTIONS = \
-Wall

CODE_GENERATION_OPTIONS = \
-fcheck=all

