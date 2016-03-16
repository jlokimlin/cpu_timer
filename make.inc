
LIB_NAME = cpu_timer

LIB = ../lib/lib$(LIB_NAME).a

AR = /usr/bin/ar

MAKE = /usr/bin/make

#EXTERNAL_LIBRARY_PATH = /usr/local/lib
EXTERNAL_LIBRARY_PATH = /usr/local/lib64

BIN_PATH = /usr/local/bin

OBJ_DIRECTORY = ../objs

LIB_DIRECTORY = ../lib

OBJ := $(patsubst %.f90, %.o, $(wildcard *.f90))

OBJS = $(addprefix $(OBJ_DIRECTORY)/, $(OBJ))

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
