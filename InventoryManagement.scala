import scala.collection.mutable
import scala.util.{Try, Success, Failure}


object inventory extends App {
  // Define the Product case class
  case class Product(id: Int, name: String, quantity: Int, price: Double)

  // Type alias for Inventory
  type Inventory = Map[Int, Product]

  // Function to retrieve all product names from inventory
  def retrieveProductNames(inventory: Inventory): Seq[String] =
    inventory.values.map(_.name).toSeq

  // Function to calculate the total value of all products in inventory
  def totalValue(inventory: Inventory): Double =
    inventory.values.map(p => p.quantity * p.price).sum

  // Function to check if the inventory is empty
  def isEmpty(inventory: Inventory): Boolean =
    inventory.isEmpty

  // Function to merge two inventories
  def mergeInventories(inventory1: Inventory, inventory2: Inventory): Inventory =
    (inventory1 ++ inventory2).map { case (id, p2) =>
      val p1 = inventory1.getOrElse(id, Product(id, p2.name, 0, 0))
      id -> Product(id, p2.name, p1.quantity + p2.quantity, math.max(p1.price, p2.price))
    }

  // Function to get details of a product by its ID
  def getProductDetails(inventory: Inventory, productId: Int): Option[Product] =
    inventory.get(productId)

  // Sample inventories
  val inventory1: Inventory = Map(
    101 -> Product(101, "Product A", 10, 10.0),
    102 -> Product(102, "Product B", 5, 20.0)
  )

  val inventory2: Inventory = Map(
    102 -> Product(102, "Product B", 3, 15.0),
    103 -> Product(103, "Product C", 8, 30.0)
  )

  // I. Retrieve all product names from inventory1
  println("Product names in inventory1:")
  println(retrieveProductNames(inventory1))

  // II. Calculate the total value of all products in inventory1
  println("\nTotal value of all products in inventory1:")
  println(totalValue(inventory1))

  // III. Check if inventory1 is empty
  println("\nIs inventory1 empty?")
  println(isEmpty(inventory1))

  // IV. Merge inventory1 and inventory2, updating quantities and retaining the highest price
  val mergedInventory = mergeInventories(inventory1, inventory2)
  println("\nMerged inventory:")
  println(mergedInventory)

  // V. Check if a product with a specific ID (e.g., 102) exists and print its details if it does
  val productIdToCheck = 102
  println(s"\nChecking for product with ID $productIdToCheck:")
  getProductDetails(inventory1, productIdToCheck) match {
    case Some(product) => println(s"Product details: $product")
    case None => println("Product not found")
  }
}

