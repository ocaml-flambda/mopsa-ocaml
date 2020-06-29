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
framework = ast core lattices sig combiners toplevel engines output config runner
framework.ast = program frontend typ operator constant var expr stmt visitor all
framework.core = query context zone alarm token log lattice soundness flow \
		 cases eval post id interface manager hook cache all
framework.lattices = partial_map partial_inversible_map_sig \
		     partial_inversible_map powerset_sig powerset \
		     powersetwithunder pointwise pair
framework.sig = abstraction functor reduction
framework.sig.abstraction = stacked domain simplified stateless value
framework.sig.functor = stacked domain simplified value
framework.sig.reduction = eval exec simplified value
framework.combiners = value domain
framework.combiners.value = common union product nonrel
framework.combiners.domain = sequence compose product simplified_product \
                             stateless_sequence
framework.engines = engine automatic interactive
framework.output = text json factory
framework.config = paths abstraction options
framework.config.abstraction = syntax visitor parser builder



# Packages
PKGS += str yojson gmp apron apron.boxMPQ apron.octMPQ apron.polkaMPQ zarith threads


# MOPSA libraries
MOPSALIBS += utils:utils

# this is needed for ocamlrun to find our dll*.so when running the bytecode bynary
# TODO: find a better way
DLLPATHS += -dllpath `pwd`/$(MOPSAROOT)/utils/lib
