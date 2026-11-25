package net.andimiller.torpor.client

/** The schemas that the tests write operations against. */
object LibrarySchema {
  inline val sdl = """
    enum Genre {
      FICTION
      SCIENCE
    }

    scalar Date

    input ShelfFilter {
      genre: Genre!
      minItems: Int
      or: [ShelfFilter!]
    }

    type Shelf {
      name: String!
      tags: [String!]!
      published: Date
    }

    type Query {
      shelves(filter: ShelfFilter, first: Int = 10, names: [String!]!): [Shelf!]!
    }
  """
}

object PetshopSchema {
  inline val sdl = """
    enum Species {
      DOG
      CAT
    }

    type Pet {
      id: ID!
      name: String!
      species: Species!
      age: Int
      owner: Owner
    }

    type Owner {
      name: String!
      pets: [Pet!]!
    }

    type Shelter {
      name: String!
      capacity: Int!
    }

    union Home = Owner | Shelter

    type Query {
      pets(species: Species, first: Int): [Pet!]!
      pet(id: ID!): Pet
      home(pet: ID!): Home
    }
  """
}
