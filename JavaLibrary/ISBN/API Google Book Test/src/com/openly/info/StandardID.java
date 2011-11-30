package com.openly.info;/* * *  This file has been released as part of the Openly Jake Package *  Copyright 2000  by Openly Informatics, Inc. *  http://www.openly.com/ * *  This library is free software; you can redistribute it and/or *  modify it under the terms of the GNU Lesser General Public *  License as published by the Free Software Foundation; either *  version 2 of the License, or (at your option) any later version. * *  This library is distributed in the hope that it will be useful, *  but WITHOUT ANY WARRANTY; without even the implied warranty of *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *  Lesser General Public License for more details. * *  You should have received a copy of the GNU Lesser General Public *  License along with this library; if not, write to the Free *  Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, *  MA 02111-1307, USA * *//** * StandardID wraps a Standard Identifier, such as an ISSN or ISBN.  * @author Eric Hellman * &copy; Copyright 2000 Openly Informatics, Inc. * All Rights Reserved. * */public interface StandardID {	public String toString();		/**	* @return the name of the standard identifier	*/	public String IDName();}