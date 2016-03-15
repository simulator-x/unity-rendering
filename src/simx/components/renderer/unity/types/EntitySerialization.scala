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

import org.json4s.jackson.Serialization
import simx.components.io.json.typehints._
import simx.components.renderer.unity.SerializationDebugHook
import simx.components.renderer.unity.messages.EntityUpdate
import simx.core.entity.Entity
import simx.core.entity.description.SValSet
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.ontology.types
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

object EntitySerialization {
  private var supportedTypes = Seq[StackableHint[_]](UUIDHint)
  private var reverseTypeMapping = Map[String, ConvertibleTrait[_]]()
  private var hintMapping = Map[String, StackableHint[_]]()
  hintMapping = hintMapping.updated("ConstMat4f", ConstMat4fHint)
  hintMapping = hintMapping.updated("ConstQuat4f", ConstQuat4fHint)
  hintMapping = hintMapping.updated("ConstVec3f", ConstVec3fHint)
  hintMapping = hintMapping.updated("float", JSonFloatHint)

  def addType[T, U](c: ConvertibleTrait[T], hint: StackableHint[U]) {
    supportedTypes = supportedTypes ++ Seq(hint)
    reverseTypeMapping = reverseTypeMapping.updated(c.semantics.toString.toLowerCase, c)
  }

  def reverseLookup(semantics: String) = reverseTypeMapping.get(semantics.toString.toLowerCase)


  def addTypeIfNew[T](c: ConvertibleTrait[T]) {
    hintMapping.get(c.classTag.runtimeClass.getSimpleName) match {
      case Some(hint: StackableHint[T@unchecked]) =>
        if (!supportedTypes.contains(hint)) {
          addType(c, hint)
        }
      case _ =>
    }
  }

  addType(types.Transformation, ConstMat4fHint)
  addType(Rotation, ConstQuat4fHint)
  addType(Position, ConstVec3fHint)
  addType(Scale, ConstVec3fHint)

  private implicit val hint = TypeHints.shortHints(classOf[EntityUpdate] :: Nil, supportedTypes: _*)

  def serialize(e: Entity, values: SValSet, toGet: ConvertibleTrait[_]*) = {
    SerializationDebugHook.write(EntityUpdate(e.id, toGet.map(tg => values.getFirstSValFor(tg)).map {
      tuple => tuple.get.typedSemantics.semantics.toString -> tuple.get.value
    }.toMap))
  }

  def serializeAll(e: Entity, values: SValSet) =
    serialize(e, values, values.values.flatten.map(_.typedSemantics.asConvertibleTrait).toSeq :_*)

  def deserialize(jsonString: String, getEntity: java.util.UUID => Option[Entity])(handler: Entity => Any)(implicit context: EntityUpdateHandling) {
    val readEntity = Serialization.read(jsonString)(hint, manifest[EntityUpdate])
    getEntity(readEntity.id) match {
      case None => println("error: id mismatch while deserializing")
      case Some(entity) =>
        val toSet = readEntity.data.keys.map(key => readEntity.getData(reverseTypeMapping(key)))
        if (toSet.nonEmpty) {
          toSet.tail.foreach(entity.set(_))
          entity.set(toSet.head, handler)
        } else
          handler(entity)

    }
  }
}
