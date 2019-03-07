##############################################################################
#                                                                            #
#  This file is part of MOPSA, a Modular Open Platform for Static Analysis.  #
#                                                                            #
#  Copyright (C) 2017-2019 The MOPSA Project.                                #
#                                                                            #
#  This program is free software: you can redistribute it and/or modify      #
#  it under the terms of the GNU Lesser General Public License as published  #
#  by the Free Software Foundation, either version 3 of the License, or      #
#  (at your option) any later version.                                       #
#                                                                            #
#  This program is distributed in the hope that it will be useful,           #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#  GNU Lesser General Public License for more details.                       #
#                                                                            #
#  You should have received a copy of the GNU Lesser General Public License  #
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
#                                                                            #
##############################################################################

TARGET_CLIBS = utils

utils = bot compare debug dnf equiv setExt setExtSig setP invRelation invRelationSig listExt mapExtSig mapExt mapPolyHet mapP option location printers exceptions relation relationSig valueSig timing top itvUtils congUtils bitfields graphSig graph typeExt eq
itvUtils = float intBound intItv floatItv floatItvNan

# C files
C_SRC = itvUtils/floats_x86_64.c

# External packages, as given to ocamlfind
PKGS = str gmp zarith camlidl


MOPSAROOT = ..

include $(MOPSAROOT)/make/main.mk
