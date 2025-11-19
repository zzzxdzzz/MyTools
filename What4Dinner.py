"""
Simple dinner suggestion helper extracted from What4Dinner.ipynb.
Use `suggest_meals` to filter by cuisine, protein, diet, cook time, or servings,
and `format_meals` to print a readable summary.
"""
import random
from textwrap import dedent
from typing import List, Optional, Sequence

random.seed(42)

MEALS = [
    {
        "name": "Lemon Herb Roast Chicken",
        "cuisine": "Mediterranean",
        "protein": "chicken",
        "diet": "standard",
        "cook_time": 60,
        "servings": 4,
        "notes": "Serve with roasted potatoes and a simple salad.",
    },
    {
        "name": "Tofu Stir-Fry with Ginger Soy Glaze",
        "cuisine": "Asian",
        "protein": "tofu",
        "diet": "vegetarian",
        "cook_time": 25,
        "servings": 2,
        "notes": "Use any crunchy veggies on hand—bell peppers, carrots, broccoli.",
    },
    {
        "name": "Chickpea and Spinach Curry",
        "cuisine": "Indian",
        "protein": "chickpea",
        "diet": "vegan",
        "cook_time": 35,
        "servings": 4,
        "notes": "Finish with coconut milk; great with rice or naan.",
    },
    {
        "name": "Shrimp Tacos with Cilantro Lime Slaw",
        "cuisine": "Mexican",
        "protein": "shrimp",
        "diet": "pescatarian",
        "cook_time": 30,
        "servings": 3,
        "notes": "Quick sear the shrimp and warm tortillas just before serving.",
    },
    {
        "name": "Garlic Butter Salmon and Green Beans",
        "cuisine": "American",
        "protein": "salmon",
        "diet": "pescatarian",
        "cook_time": 22,
        "servings": 2,
        "notes": "Sheet-pan friendly; broil for the last 2 minutes for color.",
    },
    {
        "name": "Beef Bulgogi Rice Bowls",
        "cuisine": "Korean",
        "protein": "beef",
        "diet": "standard",
        "cook_time": 30,
        "servings": 4,
        "notes": "Thinly slice the beef and marinate for at least 15 minutes.",
    },
    {
        "name": "Mushroom Risotto",
        "cuisine": "Italian",
        "protein": "mushroom",
        "diet": "vegetarian",
        "cook_time": 40,
        "servings": 4,
        "notes": "Use warmed stock and finish with parmesan and lemon zest.",
    },
    {
        "name": "Turkey Lettuce Wraps",
        "cuisine": "Asian",
        "protein": "turkey",
        "diet": "standard",
        "cook_time": 20,
        "servings": 3,
        "notes": "Add water chestnuts for crunch and hoisin for sweetness.",
    },
    {
        "name": "Black Bean Sweet Potato Bowls",
        "cuisine": "Mexican",
        "protein": "black bean",
        "diet": "vegan",
        "cook_time": 35,
        "servings": 4,
        "notes": "Roast sweet potatoes with smoked paprika; top with avocado.",
    },
    {
        "name": "Seared Tuna Nicoise Salad",
        "cuisine": "Mediterranean",
        "protein": "tuna",
        "diet": "pescatarian",
        "cook_time": 25,
        "servings": 2,
        "notes": "Keep eggs jammy and blanch green beans for 3 minutes.",
    },
]


def suggest_meals(
    cuisine: Optional[str] = None,
    protein: Optional[str] = None,
    diet: Optional[str] = None,
    max_cook_time: Optional[int] = None,
    servings: Optional[int] = None,
    *,
    limit: int = 5,
) -> List[dict]:
    """Return matching meal ideas sorted by cook time."""

    meals: Sequence[dict] = MEALS
    if cuisine:
        meals = [m for m in meals if m["cuisine"].lower() == cuisine.lower()]
    if protein:
        meals = [m for m in meals if m["protein"].lower() == protein.lower()]
    if diet:
        meals = [m for m in meals if m["diet"].lower() == diet.lower()]
    if max_cook_time:
        meals = [m for m in meals if m["cook_time"] <= max_cook_time]
    if servings:
        meals = [m for m in meals if m["servings"] >= servings]

    meals = sorted(meals, key=lambda m: m["cook_time"])
    return list(meals[:limit])


def format_meals(meals: Sequence[dict]) -> str:
    """Format meal suggestions for display."""
    if not meals:
        return "No matches—try relaxing a filter."

    lines = []
    for m in meals:
        lines.append(
            dedent(
                f"""
        - {m['name']} ({m['cuisine']}, {m['protein']}, {m['diet']})
          time: {m['cook_time']} min | serves {m['servings']}
          note: {m['notes']}
        """
            ).strip()
        )
    return "\n".join(lines)


def pick_random_meal() -> str:
    """Return a single random meal suggestion, formatted for display."""
    return format_meals([random.choice(MEALS)])


if __name__ == "__main__":
    # Example: quick pescatarian meals in ~30 minutes
    top_matches = suggest_meals(diet="pescatarian", max_cook_time=30)
    print("Quick pescatarian ideas:\n" + format_meals(top_matches))

    # Pick a random idea for tonight
    print("\nRandom pick:\n" + pick_random_meal())
