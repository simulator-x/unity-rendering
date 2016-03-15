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

package simx.components.renderer.unity

import simx.core.ontology.{SValDescription, Symbols}
import simx.core.ontology.types.{Position, Direction, Intensity, OntologySymbol}

import simx.core.worldinterface.eventhandling.EventDescription

/**
 * Created by dwiebusch on 17.05.14
 */
package object types {
  object Position extends simx.core.ontology.SValDescription(simx.core.ontology.types.Transformation as Symbols.position withType classOf[simplex3d.math.float.ConstVec3] definedAt "simx.components.renderer.unity")
  object Scale extends simx.core.ontology.SValDescription(simx.core.ontology.types.Transformation as Symbols.scale withType classOf[simplex3d.math.float.ConstVec3] definedAt "simx.components.renderer.unity")
  object Rotation extends simx.core.ontology.SValDescription(simx.core.ontology.types.Transformation as UnitySymbols.rotation withType classOf[simplex3d.math.float.ConstQuat4] definedAt "simx.components.renderer.unity")
  object Temperature extends SValDescription(simx.core.ontology.types.Real as UnitySymbols.temperature)
}

object UnitySymbols{
  object rotation extends OntologySymbol('Rotation)
  object temperature extends OntologySymbol(Symbol("Temperature"))
  object onCollisionEnter extends OntologySymbol('OnCollisionEnter)
  object onCollisionStay extends OntologySymbol('OnCollisionStay)
  object onCollisionExit extends OntologySymbol('OnCollisionExit)
  object onTriggerEnter extends OntologySymbol('OnTriggerEnter)
  object onTriggerStay extends OntologySymbol('OnTriggerStay)
  object onTriggerExit extends OntologySymbol('OnTriggerExit)
  object applyForce extends OntologySymbol('ApplyForce)
  object applyImpulse extends OntologySymbol('ApplyImpulse)
}

object UnityEvents{
  val OnCollisionEnter = new EventDescription(UnitySymbols.onCollisionEnter)
  val OnCollisionStay = new EventDescription(UnitySymbols.onCollisionStay)
  val OnCollisionExit = new EventDescription(UnitySymbols.onCollisionExit)
  val OnTriggerEnter = new EventDescription(UnitySymbols.onTriggerEnter)
  val OnTriggerStay = new EventDescription(UnitySymbols.onTriggerStay)
  val OnTriggerExit = new EventDescription(UnitySymbols.onTriggerExit)
  val ApplyForce = new EventDescription(UnitySymbols.applyForce, List(Intensity, Direction, Position))
  val ApplyImpulse = new EventDescription(UnitySymbols.applyImpulse, List(Intensity, Direction, Position))
}