package reader.model

import net.liftweb.mapper._

class EnvironmentValue extends LongKeyedMapper[EnvironmentValue] with IdPK {

    def getSingleton = EnvironmentValue

    object key extends MappedString(this, 255)

    object value extends MappedString(this, 255)

    def setValue(v: String) = value(v).saveMe()
}

object EnvironmentValue extends EnvironmentValue with LongKeyedMetaMapper[EnvironmentValue] {
    override def dbTableName = "environmentvalues"

    def apply(key: String, default: String) = {
        this.synchronized{
            EnvironmentValue.find(By(EnvironmentValue.key, key))
                .openOr(EnvironmentValue.create
                    .key(key)
                    .value(default)
                    .saveMe())
        }
    }
}