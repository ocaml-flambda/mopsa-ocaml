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


# Framework packs
framework = ast core lattices transformers output config hooks engines runner
framework.ast = program frontend typ operator constant var expr stmt visitor all
framework.core = query context zone callstack alarm token log lattice        \
		 soundness flow cases eval post id interface channel sig    \
                 hook cache abstraction all
framework.core.sig = value domain stacked functor
framework.core.sig.value = lowlevel simplified reduction
framework.core.sig.domain = manager lowlevel intermediate simplified         \
                            stateless reduction
framework.core.sig.stacked = manager lowlevel intermediate stateless reduction
framework.core.sig.functor = simplified
framework.lattices = partial_map partial_inversible_map_sig partial_inversible_map powerset_sig powerset powersetwithunder
framework.engines = engine automatic interactive
framework.transformers = value domain stacked
framework.transformers.stacked.lowlevel = stack_list compose product sequence
framework.transformers.stacked.intermediate = stack_list product sequence
framework.output = text json factory
framework.config = paths visitor typer parser options
framework.hooks = logs



# Packages
PKGS += str yojson gmp apron apron.boxMPQ apron.octMPQ apron.polkaMPQ zarith threads


# MOPSA libraries
MOPSALIBS += utils:utils

# this is needed for ocamlrun to find our dll*.so when running the bytecode bynary
# TODO: find a better way
DLLPATHS += -dllpath `pwd`/$(MOPSAROOT)/utils/lib
