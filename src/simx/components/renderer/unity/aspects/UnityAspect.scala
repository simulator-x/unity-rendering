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

package simx.components.renderer.unity.aspects

import simx.core.entity.description.EntityAspect
import simx.core.ontology.{types, GroundedSymbol, Symbols}
import simx.core.entity.typeconversion.ConvertibleTrait
import simplex3d.math.float._
import simx.core.entity.description.NamedSValSet

/**
 *
 * Created by dennis on 13.05.14.
 */
abstract class UnityAspect(aspectType : GroundedSymbol) extends EntityAspect(Symbols.graphicsComponent, aspectType, Nil)

//case class UnitySphere() extends UnityAspect(Symbols.sphere){
//  /**
//   * the features the entity will at least have when it is created
//   * @return the features the entity will at least have when it is created
//   */
//  override def getFeatures: Set[ConvertibleTrait[_]] = Set(types.Transformation)
//
//  /**
//   * the features the component for which this aspect is designed *must* provide (i.e. provide initial values for those
//   * features)``
//   * @return a set of features
//   */
//  override def getProvidings: Set[ConvertibleTrait[_]] = Set()
//
//  /**
//   *
//   * The list of create parameters
//   * @return a list of [[simx.core.entity.description.SVal]]'s containing the information needed to instantiate an
//   *         entity with this aspect
//   */
//  override def getCreateParams: NamedSValSet =
//    NamedSValSet(aspectType)
//}

case class UnityAsset(string : String, scale : ConstMat4) extends UnityAspect(Symbols.file){
  /**
   * the features the entity will at least have when it is created
   * @return the features the entity will at least have when it is created
   */
  override def getFeatures: Set[ConvertibleTrait[_]] = Set()

  /**
   * the features the component for which this aspect is designed *must* provide (i.e. provide initial values for those
   * features)``
   * @return a set of features
   */
  override def getProvidings: Set[ConvertibleTrait[_]] = Set()

  /**
   *
   * The list of create parameters
   * @return a list of [[simx.core.entity.description.SVal]]'s containing the information needed to instantiate an
   *         entity with this aspect
   */
  override def getCreateParams: NamedSValSet =
    NamedSValSet(aspectType, types.String.withAnnotations(Symbols.file)(string), types.Scale(scale))
}

case class UnityExistingNode(name : String, offsets : ConstMat4 = Mat4.Identity) extends UnityAspect(Symbols.existingNode){
  /**
   * the features the entity will at least have when it is created
   * @return the features the entity will at least have when it is created
   */
  override def getFeatures: Set[ConvertibleTrait[_]] = Set(types.Transformation)

  /**
   * the features the component for which this aspect is designed *must* provide (i.e. provide initial values for those
   * features)``
   * @return a set of features
   */
  override def getProvidings: Set[ConvertibleTrait[_]] = Set()//types.Transformation)

  /**
   *
   * The list of create parameters
   * @return a list of [[simx.core.entity.description.SVal]]'s containing the information needed to instantiate an
   *         entity with this aspect
   */
  override def getCreateParams: NamedSValSet =
    NamedSValSet(aspectType, types.Name(name), types.Transformation.withAnnotations(Symbols.offset)(offsets))
}