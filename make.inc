
LIB = ../lib/libcpu_timer.a

AR = /usr/bin/ar

MAKE = /usr/bin/make

# Set directories

#EXTERNAL_LIBRARY_PATH = /usr/local/lib
EXTERNAL_LIBRARY_PATH = /usr/local/lib64

BIN_PATH = /usr/local/bin

SRC_DIRECTORY = ./

SRC= $(wildcard $(SRC_DIRECTORY)/*.f90)

OBJ := $(patsubst %.f90, %.o, $(wildcard *.f90))

OBJ_DIRECTORY = ../objs

LIB_DIRECTORY = ../lib

OBJS = $(addprefix $(OBJ_DIRECTORY)/, $(OBJ))

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

