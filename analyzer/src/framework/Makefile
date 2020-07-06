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
framework = ast core lattices abstraction engines output config runner
framework.ast = program frontend typ operator constant var expr stmt visitor all
framework.core = query context semantic alarm token log lattice soundness flow \
		 cases eval post id manager hook cache all
framework.lattices = partial_map partial_inversible_map_sig \
		     partial_inversible_map powerset_sig powerset \
		     powersetwithunder pointwise pair
framework.abstraction = sig tree combiners dag json toplevel
framework.abstraction.sig = domain functor reduction
framework.abstraction.sig.domain = stacked standard simplified stateless value
framework.abstraction.sig.functor = stacked standard simplified value
framework.abstraction.sig.reduction = eval exec simplified value
framework.abstraction.combiners = value domain
framework.abstraction.combiners.value = common union product nonrel
framework.abstraction.combiners.domain = sequence compose product \
					 simplified_product stateless_sequence
framework.abstraction.json = syntax visitor parser builder
framework.engines = engine automatic interactive
framework.output = text json factory
framework.config = paths options



# Packages
PKGS += str yojson gmp apron apron.boxMPQ apron.octMPQ apron.polkaMPQ zarith threads


# MOPSA libraries
MOPSALIBS += utils:utils

# this is needed for ocamlrun to find our dll*.so when running the bytecode bynary
# TODO: find a better way
DLLPATHS += -dllpath `pwd`/$(MOPSAROOT)/utils/lib
