/*
 * Copyright 2014 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the 
 * HCI Group at the University of Würzburg. The project is funded by the German 
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.renderer.unity.types

import simx.core.entity.typeconversion.Converter
import simx.core.ontology.types
import simplex3d.math.float._
import simplex3d.math.floatx.ConstMat4f
import simplex3d.math.floatx.functions._

/**
 * Created by dwiebusch on 17.05.14
 */
object UnityConverters {
  new Converter(Position)(types.Transformation){
    /**
     * the actual conversion function
     * @param i the input data to be converted
     * @return the converted data
     */
    override def convert(i: ConstVec3): ConstMat4 =
      ConstMat4(Mat4x3.translate(ConstVec3(i.x, i.y, -i.z)))

    /**
     * the actual reversion function
     * @param i the input data to be reverted
     * @return the reverted data
     */
    override def revert(i: ConstMat4): ConstVec3 =
      ConstVec3(i(3).x, i(3).y, -i(3).z)
  }

  new Converter(Scale)(types.Transformation){
    /**
     * the actual conversion function
     * @param i the input data to be converted
     * @return the converted data
     */
    override def convert(i: ConstVec3): ConstMat4 =
      ConstMat4(Mat4x3.scale(i))

    /**
     * the actual reversion function
     * @param i the input data to be reverted
     * @return the reverted data
     */
    override def revert(i: ConstMat4): ConstVec3 =
      ConstVec3(length(i(0).xyz), length(i(1).xyz), length(i(2).xyz))

  }

  new Converter(Rotation)(types.Transformation){
    /**
     * the actual conversion function
     * @param i the input data to be converted
     * @return the converted data
     */
    override def convert(i: ConstQuat4): ConstMat4f =
      ConstMat4f(rotationMat(Quat4(i.d, -i.a, -i.b, i.c)))

    /**
     * the actual reversion function
     * @param i the input data to be reverted
     * @return the reverted data
     */
    override def revert(i: ConstMat4f): ConstQuat4 = {
      val tmp = quaternion(Mat3(i))
      ConstQuat4(tmp.d, -tmp.a, -tmp.b, tmp.c)
    }
  }

}
