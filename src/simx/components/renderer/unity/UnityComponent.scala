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

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets

import akka.util.ByteString
import org.json4s.{Formats, MappingException}
import org.json4s.jackson.Serialization
import simplex3d.math.float.{ConstMat4, ConstVec3, Mat4}
import simplex3d.math.floatx.functions
import simx.components.io.json.net._
import simx.components.io.json.settings.JacksonSettings
import simx.components.io.json.typehints._
import simx.components.renderer.unity.messages._
import simx.components.renderer.unity.types.{EntitySerialization, UnityConverters}
import simx.core.component.Component
import simx.core.components.physics.PhysicsEvents
import simx.core.entity.Entity
import simx.core.entity.component.ComponentAspect
import simx.core.entity.description.SVal.SValType
import simx.core.entity.description.{EntityAspect, NamedSValSet, SValSet}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.ontology.types.{Direction, Intensity, Position}
import simx.core.ontology.{Symbols, types => gt}
import simx.core.svaractor.{MultiObserve, SVarActor}
import simx.core.worldinterface.eventhandling.{Event, EventHandler, EventProvider}
import scala.reflect.{ClassTag, classTag}



case class UnityComponentAspect(name: Symbol, address: String, port: Int, providedSVars: Set[ConvertibleTrait[_]] = Set(gt.Transformation, gt.Scale), receivedSVars: Set[ConvertibleTrait[_]] = Set())
  extends ComponentAspect[UnityComponent](Symbols.graphicsComponent, name, address :: port :: providedSVars :: receivedSVars :: Nil) {
  def getComponentFeatures: Set[ConvertibleTrait[_]] = Set()

  def getCreateParams: NamedSValSet = NamedSValSet(aspectType)
}

/**
 *
 * Created by dennis on 13.05.14.
 */
class UnityComponent(name: Symbol = 'unity, address: String = "localhost", port: Int = 8000,
                     providedSVars: Set[ConvertibleTrait[_]] = Set(gt.Transformation, gt.Scale),
                     receivedSVars: Set[ConvertibleTrait[_]] = Set())
  extends Component(componentName = name, componentType = Symbols.graphicsComponent) with MultiObserve with EventProvider with EventHandler {
  JacksonSettings.init()

  JacksonSettings.init()

  StackableHint.registerOverride(classTag[ApplyForce], "AF")
  StackableHint.registerOverride(classTag[EntityUpdate], "EU")
  StackableHint.registerOverride(classTag[ExistingObject], "EO")

  provideEvent(UnityEvents.OnCollisionEnter)
  provideEvent(UnityEvents.OnCollisionExit)
  provideEvent(UnityEvents.OnCollisionStay)
  provideEvent(UnityEvents.OnTriggerEnter)
  provideEvent(UnityEvents.OnTriggerExit)
  provideEvent(UnityEvents.OnTriggerStay)
  requestEvent(UnityEvents.ApplyForce)
  requestEvent(UnityEvents.ApplyImpulse)

  providedSVars foreach {
    p =>
      EntitySerialization.addTypeIfNew(p)
  }
  receivedSVars foreach {
    r =>
      EntitySerialization.addTypeIfNew(r)
  }

  private var client: Option[SVarActor.Ref] = None
  UnityConverters

  /**
   * (re)configure the component
   * @param params the configuration params
   */
  override protected def configure(params: SValSet) {
  }

  /**
   * called when the construction ot the entity representing this component is completed
   * @param e the entity representing this component
   */
  override protected def finalizeConfiguration(e: Entity) {
  }

//  addHandler[Disconnected] {
//    msg => client = None
//      val name = "log_" + System.currentTimeMillis()
//      var string = ""
//      for (t <- log) {
//        string += t._1 + "," + t._2 + " "
//      }
//      File(name).writeAll(string)
//  }

  addHandler[Disconnect] {
    msg => client = None
  }

  addHandler[WriteFailed] {
    msg =>
      println("[UnityComponent] ERROR: Sending message to server failed")
      client = None
  }

  protected val supportedMessages = List(classOf[ExistingObject], classOf[EntityUpdate], classOf[CollisionEvent], classOf[IDAssociation], classOf[TemperatureUpdate], classOf[ApplyForce])
  protected var offsets = Map[Entity, ConstMat4]()

  addHandler[JSONString] {
    msg =>
      implicit val hint = TypeHints.shortHints(supportedMessages, UUIDHint, ConstVec3fHint, ConstQuat4fHint, StringHint, JSonFloatHint)
      try {
        Serialization.read[Any](msg.str) match {
          case obj: ExistingObject => handleExistingObject(obj)
          case update: EntityUpdate => update.updateEntity(knownEntities.get, offsets, receivedSVars)
          case CollisionEvent(state, id1, id2) =>
            associatedIds.get(id1).collect {
              case entity1 =>
                associatedIds.get(id2).collect {
                  case entity2 =>
                    handleCollision(state, entity1, entity2)
                }
            }

          case IDAssociation(id, unityId) => associatedIds += unityId -> id
          case unsupported => println("[WARN:][" + componentName.name + "] unsupported msg: " + unsupported)
        }
      } catch {
        case MappingException(errorMsg, e) => println("failed to process " + msg + "\n\tcaused by " + errorMsg)
      }
  }

  override def handleEvent(e: Event) = {
    e.name match {
      case UnityEvents.ApplyForce.name =>
        for (entity <- e.affectedEntities) {
          e.get(Intensity) match {
            case Some(intensity) =>
              e.get(Direction) match {
                case Some(direction) =>
                  e.get(Position) match {
                    case Some(position) =>
                      implicit val hint = TypeHints.shortHints(classOf[ApplyForce] :: Nil, UUIDHint, ConstVec3fHint, ConstQuat4fHint, StringHint, JSonFloatHint)
                      sendJSONString(SerializationDebugHook.write(ApplyForce(entity.id, intensity, direction)))

                    case None =>
                  }
                case None =>
              }
            case None =>
          }
        }
      case UnityEvents.ApplyImpulse.name =>
        for (entity <- e.affectedEntities) {
          e.get(Intensity) match {
            case Some(intensity) =>
              e.get(Direction) match {
                case Some(direction) =>
                  e.get(Position) match {
                    case Some(position) =>
                      implicit val hint = TypeHints.shortHints(classOf[ApplyForce] :: Nil, UUIDHint, ConstVec3fHint, ConstQuat4fHint, StringHint, JSonFloatHint)
                      sendJSONString(SerializationDebugHook.write(ApplyForce(entity.id, intensity, direction)))
                      //log :+= ("out", System.nanoTime())
                    case None =>
                  }
                case None =>
              }
            case None =>
          }
        }
      case _ =>
    }
  }

  //var log: List[(String, Long)] = List()
  var lastEvent = ("", new Entity(), new Entity()) // TODO! very bad hack!

  protected def handleCollision(state: String, id1: java.util.UUID, id2: java.util.UUID) {
    knownEntities.get(id1).collect {
      case entity1 =>
        knownEntities.get(id2).collect {
          case entity2 =>
            state match {
              case "col_enter" =>
                if (!(lastEvent._1.equals("col_enter") && lastEvent._2.id.equals(entity2.id) && lastEvent._3.id.equals(entity1.id))) {
                  UnityEvents.OnCollisionEnter.emit(Set(entity1, entity2))
                  lastEvent = ("col_enter", entity1, entity2)
                }
              case "col_exit" =>
                if (!(lastEvent._1.equals("col_exit") && lastEvent._2.id.equals(entity2.id) && lastEvent._3.id.equals(entity1.id))) {
                  UnityEvents.OnCollisionExit.emit(Set(entity1, entity2))
                  lastEvent = ("col_exit", entity1, entity2)
                }
              case "col_stay" =>
                if (!(lastEvent._1.equals("col_stay") && lastEvent._2.id.equals(entity2.id) && lastEvent._3.id.equals(entity1.id))) {
                  UnityEvents.OnCollisionStay.emit(Set(entity1, entity2))
                  lastEvent = ("col_stay", entity1, entity2)
                }
              case "trig_enter" =>
                if (!(lastEvent._1.equals("trig_enter") && lastEvent._2.id.equals(entity2.id) && lastEvent._3.id.equals(entity1.id))) {
                  UnityEvents.OnTriggerEnter.emit(Set(entity1, entity2))
                  lastEvent = ("trig_enter", entity1, entity2)
                }
              case "trig_exit" =>
                if (!(lastEvent._1.equals("trig_exit") && lastEvent._2.id.equals(entity2.id) && lastEvent._3.id.equals(entity1.id))) {
                  UnityEvents.OnTriggerExit.emit(Set(entity1, entity2))
                  lastEvent = ("trig_exit", entity1, entity2)
                }
              case "trig_stay" =>
                if (!(lastEvent._1.equals("trig_stay") && lastEvent._2.id.equals(entity2.id) && lastEvent._3.id.equals(entity1.id))) {
                  UnityEvents.OnTriggerStay.emit(Set(entity1, entity2))
                  lastEvent = ("trig_stay", entity1, entity2)
                  //log :+= ("in", System.nanoTime())
                }
              case _ =>
            }
          //println("[Unity] Collision-" + state + " between " + entity1.getSimpleName + " and " + entity2.getSimpleName)
        }
    }
  }

  protected var associatedIds = Map[Int, java.util.UUID]()
  protected var knownExistingIds = Map[Int, ExistingObject]()
  protected var knownExistingNames = Map[String, ExistingObject]()
  protected var waitingForExisting = Map[String, List[ExistingObject => Any]]()
  protected var knownEntities = Map[java.util.UUID, Entity]()

  private var cam: Option[Entity] = None

  private def handleExistingObject(msg: ExistingObject) {
    if (msg.objectType == "UnityEngine.GameObject" && msg.name == "Main Camera")
      createCameraEntity(msg)
    else {
      if (msg.semantics.get("groundedsymbol").isDefined) {
        associatedIds.get(msg.id).collect {
          case uuid =>

            implicit val hint = TypeHints.shortHints(classOf[ConnectExisting] :: classOf[RequestEvent] :: Nil, UUIDHint)
            sendJSONString(SerializationDebugHook.write(RequestEvent(uuid, PhysicsEvents.collision.name.value.toString)))
        }
      }
    }
    knownExistingIds = knownExistingIds.updated(msg.id, msg)
    knownExistingNames = knownExistingNames.updated(msg.name, msg)
    waitingForExisting.getOrElse(msg.name, List()).foreach {
      func => func.apply(msg)
    }
  }

  private def createCameraEntity(msg: ExistingObject) {
    val entity = new Entity()
    entity.set(gt.Transformation(msg.get(gt.Transformation).typedValue))
    handleTransformationUpdate(entity)
    cam = Some(entity)
    implicit val hint = TypeHints.shortHints(classOf[ConnectExisting] :: Nil, UUIDHint)
    sendJSONString(SerializationDebugHook.write(ConnectExisting(entity.id, msg.id)))
    registerEntity('unity :: 'camera :: Symbol("1") :: Nil, entity)
  }

  /**
   * provide initial values for this component, similar to requestInitialValues for entities
   * @param toProvide the values to be provided
   * @param aspect the component aspect
   * @param e the entity which will represent this component later on
   * @return a SValSet containing the initial configuration for this component
   */
  override protected def requestInitialConfigValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity): SValSet = {
    client = Some(createActor(Client.props(new InetSocketAddress(address, port), self))(_ => {})(_ => {}))
    SValSet()
  }

  /**
   * method to be implemented by each component. Will be called when an entity has to be removed from the
   * internal representation.
   * @param e the Entity to be removed
   */
  override protected def removeFromLocalRep(e: Entity) {
    implicit val hint = TypeHints.shortHints(classOf[DeleteObject] :: Nil, UUIDHint, ConstVec3fHint)
    sendJSONString(SerializationDebugHook.write(DeleteObject(e.id)))
  }

  private var observedEntities = Map[java.util.UUID, Entity]()

  /**
   * used to integrate the entity into the simulation
   * @param e the entity to be integrated
   * @param aspect the aspect which the component has to process
   */
  override protected def entityConfigComplete(e: Entity, aspect: EntityAspect) {
    knownEntities += e.id -> e
    handleTransformationUpdate(e)
  }

  private def handleTransformationUpdate(e: Entity) {
    observedEntities = observedEntities.updated(e.id, e)
    providedSVars foreach {
      c =>
        if (c.equals(gt.Transformation)) {
          e.observe(gt.Transformation, Set(self)).head {
            newTrafo =>
              val withOffset = newTrafo * functions.inverse(offsets.getOrElse(e, Mat4.Identity))
              val newPos = types.Position(gt.Transformation.convert(types.Position)(withOffset))
              val newRot = types.Rotation(gt.Transformation.convert(types.Rotation)(withOffset))
              //val newScale = types.Scale(gt.Transformation.convert(types.Scale)(withOffset))
              sendJSONString(EntitySerialization.serialize(e, SValSet(newPos, newRot), types.Position, types.Rotation))
          }
        } else {
          observeSVar(e, c)
        }
    }
  }

  private def observeSVar[T](e: Entity, c: ConvertibleTrait[T]): Unit = {
    e.observe(c, Set(self)).head {
      newC =>
        sendJSONString(EntitySerialization.serialize(e, SValSet(c(newC)), c))
    }
  }

  private def sendJSONString(str: String) {
    client.foreach(_ ! ByteString(str.getBytes(Client.getEncoding)))
  }

//  protected var openNodeRequests = Map[java.util.UUID, (Entity, EntityAspect)]()

//  protected def handleNodeReply(id: java.util.UUID) {
//    val tuple = openNodeRequests(id)
//    provideInitialValues(tuple._1, SValSet())
//  }

  /**
   * provideInitialValues has to be called within this method with the full set of initial values to be provided
   * @note the component should create its local representation within this method
   * @param toProvide the convertibletraits for which values shall be provided
   * @param aspect the aspect providing the context for this method call
   * @param e the entity to be filled
   * @param given a set of create parameters that were already provided
   *
   */
  override protected def requestInitialValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity, given: SValSet) {
    if (aspect.aspectType equals Symbols.sphere) {
      implicit val hint = TypeHints.shortHints(classOf[CreateSphere] :: Nil, UUIDHint, ConstVec3fHint)
      val transformation = given.getFirstValueFor(gt.Transformation)
      val position = ConstVec3(transformation.get.m30, transformation.get.m31, -transformation.get.m32)
      sendJSONString(SerializationDebugHook.write(CreateSphere(e.id, position)))
      provideInitialValues(e, aspect.getCreateParams.combineWithValues(toProvide)._1)
    }
    else if (aspect.aspectType equals Symbols.file) {
      implicit val hint = TypeHints.shortHints(classOf[CreateAsset] :: Nil, UUIDHint, ConstVec3fHint)
      //val transformation = aspect.getCreateParams.getFirstValueFor(gt.Transformation)
      val scaleMat = aspect.getCreateParams.getFirstValueFor(gt.Scale)
      val scale = ConstVec3(scaleMat.get.m00, scaleMat.get.m11, scaleMat.get.m22)
      val path = aspect.getCreateParams.getFirstValueFor(gt.String.withAnnotations(Symbols.file)).get
      sendJSONString(SerializationDebugHook.write(CreateAsset(e.id, path, scale)))
      provideInitialValues(e, aspect.getCreateParams.combineWithValues(toProvide)._1)
    }
    else if (aspect.aspectType equals Symbols.existingNode) {
      implicit val hint = TypeHints.shortHints(classOf[RequestObjectsNamed] :: Nil, UUIDHint)
      val name = aspect.getCreateParams.getFirstValueFor(gt.Name).get
      offsets = offsets.updated(e, aspect.getCreateParams.getFirstValueForOrElse(gt.Transformation.withAnnotations(Symbols.offset))(Mat4.Identity))
      knownExistingNames get name match {
        case Some(existing) => handleReply(toProvide, e, aspect, given)(existing)
        case None =>
          waitingForExisting =
            waitingForExisting.updated(name, handleReply(toProvide, e, aspect, given) _ :: waitingForExisting.getOrElse(name, Nil))
          sendJSONString(SerializationDebugHook.write(RequestObjectsNamed(e.id, name)))
      }
    }
  }

  def handleReply(toProvide: Set[ConvertibleTrait[_]], e: Entity, aspect: EntityAspect, given: SValSet)(info: ExistingObject) {
    def getFromGiven[T](c: ConvertibleTrait[T]): SValType[T] = c(given.getFirstValueForOrElse(c) {
      if (receivedSVars.contains(c))
        info.get(c).typedValue
      else
        throw new Exception("Trying to access non existent data field: " + c)
    })

    implicit val hint = TypeHints.shortHints(classOf[ConnectExisting] :: Nil, UUIDHint)
    sendJSONString(SerializationDebugHook.write(ConnectExisting(e.id, info.id)))
    info.setOffset(offsets(e))

    val values = toProvide.map {
      providing => getFromGiven(providing)
    }.toSeq
    provideInitialValues(e, SValSet(values: _*))
  }

  /**
   * Called for each simulation step the component should execute. The frequency with which this method is called
   * depends on the used [[simx.core.component.ExecutionStrategy]].
   */
  override protected def performSimulationStep() {
    simulationCompleted()
  }
}

object SerializationDebugHook{
  private val log = collection.mutable.Map[ClassTag[_], (Int, Long)]()
  private var lastWrite = -1L

  //enable output here
  val debug = false

  def write[T <: AnyRef : ClassTag ](toWrite : T)(implicit formats: Formats) = {
    val retVal = Serialization.write(toWrite)

    if (debug) {
      val prevVal = log.getOrElse(classTag[T], 0 -> 0L)
      log.update(classTag[T], (prevVal._1 + 1) -> (prevVal._2 + retVal.length))
      val now = System.currentTimeMillis()
      if (now - lastWrite > 1000) {
        println("debug log\n\t" + log.map(x => x._2._1 + " messages of type "+ x._1 + " using " + x._2._2 / 1000 + "kB/s").mkString("\n\t"))
        lastWrite = now
        log.clear()
      }
    }

    retVal
  }
}
