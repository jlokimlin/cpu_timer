
# Set directories
SRC_DIRECTORY = ./

SOURCES = $(wildcard $(SRC_DIRECTORY)/*.f90)

SOURCES_WITH_PREFIX = $(addprefix $(SRC_DIRECTORY)/, $(SOURCES))

OBJECTS := $(patsubst %.f90, %.o, $(wildcard *.f90))

OBJ_DIRECTORY = ../obj

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

