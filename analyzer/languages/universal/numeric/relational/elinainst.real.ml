(*
 * This file is part of MOPSA, a Modular Open Platform for Static Analysis.
 *
 * SPDX-FileCopyrightText: 2017-2024 The Mopsa Authors
 *
 * SPDX-License-Identifier: LGPL-3.0-or-later
 *
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License along
 * with this program. If not, see <http://www.gnu.org/licenses/>.
 *)

open Instances

module ElinaOct = Domain.Make(struct
    type t = Elina_oct.t
    let name = "universal.numeric.relational"
    let numeric_name = "elina_oct"
    let man = Elina_oct.manager_alloc ()
  end)

let () = register_instance (module ElinaOct : RELATIONAL)


module ElinaPoly = Domain.Make(struct
    type t = Elina_poly.loose Elina_poly.t
    let name = "universal.numeric.relational"
    let numeric_name = "elina_poly"
    let man = Elina_poly.manager_alloc_loose ()
  end)

let () = register_instance (module ElinaPoly : RELATIONAL)

