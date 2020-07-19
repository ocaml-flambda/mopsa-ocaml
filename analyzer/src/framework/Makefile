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
framework = core lattices abstraction engines output config runner
framework.core = semantic ast query context wiring alarm token log lattice soundness \
		 flow cases eval rewrite post id manager hook cache all
framework.core.ast = program frontend typ operator constant var expr stmt visitor
framework.lattices = partial_map partial_inversible_map_sig \
		     partial_inversible_map powerset_sig powerset \
		     powersetwithunder pointwise pair
framework.abstraction = sig json combiners builder toplevel
framework.abstraction.sig = domain combiner reduction
framework.abstraction.sig.domain = stacked stacked_functor \
				   standard standard_functor \
				   simplified simplified_functor \
				   stateless \
				   value value_functor
framework.abstraction.sig.combiner = stacked stacked_functor \
				     standard standard_functor \
				     simplified simplified_functor \
				     stateless
framework.abstraction.sig.reduction = eval exec simplified value
framework.abstraction.combiners = common value domain
framework.abstraction.combiners.value = union product nonrel
framework.abstraction.combiners.domain = optimized sequence compose product
framework.abstraction.combiners.domain.optimized = simplified_product \
						   stateless_sequence
framework.abstraction.json = syntax visitor parser
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
