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

package simx.components.renderer.unity.messages

import java.util.UUID

import simplex3d.math.float._
import simx.components.io.json.typehints.JSonFloat
import simx.core.entity.description.SVal.SValType
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.ontology.types
import simx.components.renderer.unity.types._
import simx.core.entity.Entity
import simx.core.ontology.types.Transformation
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

trait UnityMessage

/**
 *
 * Created by dennis on 15.05.14.
 */
case class CreateSphere(id : java.util.UUID, Position : ConstVec3) extends UnityMessage
case class CreateAsset(id : java.util.UUID, name : String, Scale : ConstVec3) extends UnityMessage
case class ConnectExisting(id : java.util.UUID, unityId : Int)
case class DeleteObject(id : java.util.UUID) extends UnityMessage
case class RequestObjectsNamed(id : java.util.UUID, name : String) extends UnityMessage
case class RequestSematic(id : java.util.UUID, name : String) extends UnityMessage
case class ProvideSematic(id : java.util.UUID, name : String, value : String) extends UnityMessage
case class RequestEvent(id : java.util.UUID, name : String) extends UnityMessage
case class CollisionEvent(state : String, id1 : Int, id2 : Int) extends UnityMessage
case class IDAssociation(id : java.util.UUID, unityId : Int) extends UnityMessage
case class TemperatureUpdate(id : java.util.UUID, Temperature: Float) extends UnityMessage
case class ApplyForce(id : java.util.UUID, Value: Float, d: ConstVec3) extends UnityMessage

sealed trait DataMap{
  val data : Map[String, Any]

  protected implicit def getValue[T](in : SValType[T]) : T = in.value

  def getData[U](tpe : ConvertibleTrait[U]) : SValType[U] =
    tpe(data(tpe.semantics.toString).asInstanceOf[tpe.dataType])

}

case class ExistingObject(id : Int, objectType : String, name: String, data : Map[String, Any], semantics : Map[String, Any]) extends UnityMessage with DataMap{
  def this(id : Int, objectType : String, name : String, data : Map[String, Any]) = this(id, objectType, name, data, Map())

  private var offset : ConstMat4 = Mat4.Identity
  def setOffset(offset : ConstMat4){ this.offset = offset }

  def get[T](c : ConvertibleTrait[T]) : SValType[T] = {

    c.semantics match {
      case types.Transformation.semantics =>

        var position : SValType[ConstVec3]  = Position(ConstVec3(0))
        var rotation : SValType[ConstQuat4] = Rotation(Quat4.Identity)
        var scale    : SValType[ConstVec3]  = Scale(ConstVec3(1))

          data.keys.foreach {
            key => EntitySerialization.reverseLookup(key) match {
              case Some(typeinfo) =>
                if (typeinfo.equals(Position))
                  position = getData(Position)
                else if (typeinfo.equals(Rotation))
                  rotation = getData(Rotation)
                else if (typeinfo.equals(Scale))
                  scale = getData(Scale)
              case None => println("could not find typeinfo for " + key)
            }
          }
          c apply (
            Transformation.convertedFrom(position) *
            Transformation.convertedFrom(rotation) *
            Transformation.convertedFrom(scale) *
            offset).toConst.asInstanceOf[T]

      case sem =>
        data.get(sem.toString) match {
          case Some(value) =>
            EntitySerialization.reverseLookup(sem.toString) match {
              case Some(typeinfo) =>
                c(value.asInstanceOf[T])
              case None =>
                throw new Exception("Existing Object: could not find typeinfo for field " + sem.toString + " in data.")
            }
          case _ =>
            throw new Exception("Existing Object: could not find field " + sem.toString + " in data.")
        }
    }
  }

}

case class EntityUpdate(id : UUID, data : Map[String, Any]) extends DataMap{
  def updateEntity(lookUp : UUID => Option[Entity], offsets : Map[Entity, ConstMat4], receivedSVars: Set[ConvertibleTrait[_]])
                  (implicit context : EntityUpdateHandling)
  {
    def putInto[T](e : Entity, c : ConvertibleTrait[T], value : Any){
      value match {
        case JSonFloat(f) =>
          e.set(c(f.asInstanceOf[T]))
        case _ =>
          e.set(c(value.asInstanceOf[T]))
      }
    }

    lookUp(id) match {
      case None =>
      case Some(entity) => data.keys.foreach {
        key => EntitySerialization.reverseLookup(key) match {
          case Some(typeinfo) =>
            if (typeinfo.equals(Position)) {
              val tmp = Transformation.convertedFrom(getData(Position)) *
                Transformation.convertedFrom(getData(Rotation)) *
                Transformation.convertedFrom(getData(Scale)) *
                offsets.getOrElse(entity, Mat4.Identity)
              entity.set(types.Transformation(tmp))
            } else if (typeinfo.equals(Rotation) || typeinfo.equals(Scale)) {
              // Do nothing! These values are handled above!
            } else  if(receivedSVars.contains(typeinfo))
              entity.set(getData(typeinfo))
          case None => println("could not find typeinfo for " + key)
        }
      }
    }
  }
}
