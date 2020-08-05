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



# Universal
lang += universal
lang.universal = ast frontend iterators heap numeric packing partitioning hooks #strings
lang.universal.iterators = unittest program intraproc interproc loops
lang.universal.iterators.interproc = common inlining #sequential_cache join_based_cache
lang.universal.heap = policies recency
lang.universal.numeric = rounding common values relational reductions
lang.universal.numeric.values = intervals congruences zero powerset
lang.universal.numeric.values.intervals = integer float
lang.universal.numeric.packing = strategy functor
lang.universal.numeric.relational = apron_manager binding apron_transformer \
                                    domain instances
lang.universal.numeric.reductions = intervals_congruences intervals_rel intervals_powerset
lang.universal.packing = static
lang.universal.partitioning = merge_after_stmt 
lang.universal.hooks = logs progress profiler gctest


# MOPSA
MOPSALIBS += parsers/universal:mlUniversalParser
