#!/usr/bin/env python3
"""
Generate Org-mode drill files for French verb topic clusters.
"""

import json
import os
import sys
from collections import defaultdict

# Topic names and descriptions based on representative verbs
TOPIC_LABELS = {
    "0": {
        "name": "Daily Activities and Pleasures",
        "description": "Verbs related to eating, sleeping, and personal care",
        "examples": [
            "Je savoure mon repas. (I savor my meal.)",
            "Elle se douche le matin. (She showers in the morning.)",
            "Nous dormons huit heures par nuit. (We sleep eight hours per night.)"
        ],
        "verbs": ["croustiller", "savourer", "doucher", "coucher", "dormir", 
                 "manger", "boire", "réveiller", "habiller", "baigner"]
    },
    "1": {
        "name": "Physical Manipulation",
        "description": "Verbs related to handling, modifying, and manipulating objects",
        "examples": [
            "Il tire sur la corde. (He pulls on the rope.)",
            "Elle froisse le papier. (She crumples the paper.)",
            "Je raccommode les vêtements déchirés. (I mend torn clothes.)"
        ],
        "verbs": ["tirailler", "froisser", "raccommoder", "sertir", "incinérer", 
                 "séparer", "couper", "plier", "lisser", "assembler"]
    },
    "2": {
        "name": "Organization and Management",
        "description": "Verbs related to organizing, arranging, and managing",
        "examples": [
            "Ils patrouillent dans la ville. (They patrol in the city.)",
            "Elle dessert la table après le repas. (She clears the table after the meal.)",
            "Le professeur chorégraphie le spectacle. (The teacher choreographs the show.)"
        ],
        "verbs": ["patrouiller", "desservir", "chorégraphier", "graduer", "empourprer",
                 "organiser", "planifier", "coordonner", "superviser", "déléguer"]
    },
    "3": {
        "name": "Technical Processes",
        "description": "Verbs related to technical or industrial processes",
        "examples": [
            "Nous nivelons le terrain. (We level the ground.)",
            "Cette machine lamine le métal. (This machine rolls the metal.)",
            "Le processus ionise les particules. (The process ionizes the particles.)"
        ],
        "verbs": ["niveler", "laminer", "ioniser", "contrôler", "rééquilibrer",
                 "calibrer", "programmer", "automatiser", "numériser", "standardiser"]
    },
    "4": {
        "name": "Gardening and Plant Life",
        "description": "Verbs related to plants, cultivation, and gardening",
        "examples": [
            "Je cueille des fleurs dans le jardin. (I pick flowers in the garden.)",
            "Il déplante les tomates trop serrées. (He uproots the tomatoes that are too close together.)",
            "Nous plantons des arbres au printemps. (We plant trees in the spring.)"
        ],
        "verbs": ["cueillir", "déplanter", "planter", "engazonner", "bêcher",
                 "arroser", "semer", "tailler", "bouturer", "greffer"]
    },
    "5": {
        "name": "Emotions and Feelings",
        "description": "Verbs related to emotions and emotional reactions",
        "examples": [
            "Son histoire m'apitoie. (His story makes me feel pity.)",
            "Cette nouvelle me courrouce. (This news angers me.)",
            "Le bruit soudain l'affole. (The sudden noise panics her.)"
        ],
        "verbs": ["apitoyer", "courroucer", "affoler", "culpabiliser", "confesser",
                 "aimer", "détester", "craindre", "s'inquiéter", "se réjouir"]
    },
    "6": {
        "name": "Arts and Creation",
        "description": "Verbs related to artistic creation and expression",
        "examples": [
            "L'artiste esquisse un portrait. (The artist sketches a portrait.)",
            "La vérité dessille ses yeux. (The truth opens his eyes.)",
            "Le sculpteur modèle l'argile. (The sculptor molds the clay.)"
        ],
        "verbs": ["esquisser", "dessiller", "modeler", "rainurer", "collectionner",
                 "peindre", "dessiner", "sculpter", "composer", "créer"]
    },
    "7": {
        "name": "Abstract Concepts",
        "description": "Verbs related to abstract and philosophical concepts",
        "examples": [
            "Le poète personnifie la mort. (The poet personifies death.)",
            "Il méconnaît son propre talent. (He fails to recognize his own talent.)",
            "L'église béatifie le saint. (The church beatifies the saint.)"
        ],
        "verbs": ["personnifier", "méconnaître", "béatifier", "réifier", "courtiser",
                "conceptualiser", "symboliser", "théoriser", "abstraire", "interpréter"]
    },
    "8": {
        "name": "Business and Communication",
        "description": "Verbs related to professional interactions and correspondence",
        "examples": [
            "La banque crédite mon compte. (The bank credits my account.)",
            "Cette section précède le chapitre final. (This section precedes the final chapter.)",
            "Nous correspondons régulièrement. (We correspond regularly.)"
        ],
        "verbs": ["créditer", "précéder", "correspondre", "agréer", "crypter",
                 "communiquer", "négocier", "facturer", "budgétiser", "investir"]
    },
    "9": {
        "name": "Casual or Colloquial Actions",
        "description": "More informal or colloquial verbs",
        "examples": [
            "Le cavalier éperonne son cheval. (The rider spurs his horse.)",
            "Elle bavasse toute la journée. (She chatters all day long.)",
            "Il pigeonne les touristes. (He dupes the tourists.)"
        ],
        "verbs": ["éperonner", "bavasser", "pigeonner", "maculer", "aiguillonner",
                "bavarder", "traînailler", "flemmarder", "rigoler", "zoner"]
    },
    "10": {
        "name": "Sound and Movement",
        "description": "Verbs related to sounds, movements, and physical sensations",
        "examples": [
            "La porte grince. (The door creaks.)",
            "Je tremble de froid. (I tremble from cold.)",
            "Le feu embrase la forêt. (The fire sets the forest ablaze.)"
        ],
        "verbs": ["grincer", "trembler", "embraser", "éclater", "enflammer",
                "claquer", "murmurer", "frissonner", "vibrer", "résonner"]
    },
    "11": {
        "name": "Physical Activity",
        "description": "Verbs related to walking, movement, and physical actions",
        "examples": [
            "Ils se castagnent dans la rue. (They fight in the street.)",
            "Nous marchons tous les jours. (We walk every day.)",
            "Le dentiste déchausse la dent. (The dentist loosens the tooth.)"
        ],
        "verbs": ["castagner", "marcher", "déchausser", "ânonner", "béquiller",
                "courir", "sauter", "nager", "grimper", "s'exercer"]
    },
    "12": {
        "name": "Professional Terminology",
        "description": "Modern business and professional verbs",
        "examples": [
            "L'entreprise mutualise les ressources. (The company pools resources.)",
            "Ils viabilisent le projet. (They make the project viable.)",
            "Le manager responsabilise son équipe. (The manager empowers his team.)"
        ],
        "verbs": ["mutualiser", "viabiliser", "responsabiliser", "tiser", "solidariser",
                "optimiser", "rentabiliser", "digitaliser", "implémenter", "réinventer"]
    },
    "13": {
        "name": "Negative Actions",
        "description": "Verbs with negative or destructive meanings",
        "examples": [
            "Quelqu'un a saboté la machine. (Someone sabotaged the machine.)",
            "Le système risque de s'autodétruire. (The system might self-destruct.)",
            "Son comportement terrifie les enfants. (His behavior terrifies the children.)"
        ],
        "verbs": ["saboter", "autodétruire", "terrifier", "pactiser", "pulluler",
                "détruire", "nuire", "effrayer", "menacer", "briser"]
    },
    "14": {
        "name": "Movement and Transformation",
        "description": "Verbs related to motion and change",
        "examples": [
            "La voiture dérouille après des années. (The car is getting back in shape after years.)",
            "Les insectes grouillent partout. (Insects swarm everywhere.)",
            "Cette nouvelle endeuille la famille. (This news puts the family in mourning.)"
        ],
        "verbs": ["dérouiller", "grouiller", "endeuiller", "ensorceler", "bruisser",
                "transformer", "métamorphoser", "évoluer", "transitionner", "changer"]
    }
}

def generate_drill_file(topic_info, output_dir):
    """Generate a single drill file for a given topic."""
    topic_name = topic_info["name"]
    topic_desc = topic_info["description"]
    examples = topic_info["examples"]
    verbs = topic_info["verbs"]
    
    # Create filename based on topic name
    basename = topic_name.lower().replace(' ', '_').replace('/', '_')
    filename = os.path.join(output_dir, f"{basename}.org")
    
    # Skip if the file already exists
    if os.path.exists(filename):
        print(f"File already exists, skipping: {filename}")
        return filename
    
    # Create the drill file content
    with open(filename, 'w', encoding='utf-8') as file:
        # Write header
        file.write(f"#+TITLE: French Verb Drill: {topic_name}\n")
        file.write("#+AUTHOR: Generated by verbiste.el\n")
        file.write("#+STARTUP: overview\n\n")
        
        # Write introduction
        file.write("* Introduction\n\n")
        file.write(f"This drill focuses on {topic_desc.lower()}.\n")
        file.write("Practice conjugating these verbs in different tenses and using them in sentences.\n\n")
        
        # List all verbs in this group
        file.write("* Verbs in this group\n\n")
        for idx, verb in enumerate(verbs):
            file.write(f"{idx+1}. {verb}\n")
        
        # Add example sentences
        file.write("\n* Example Sentences\n\n")
        for example in examples:
            file.write(f"- {example}\n")
        
        # Create drill section
        file.write("\n* Conjugation Drills\n\n")
        
        # Generate drill exercises for 3 verbs
        for verb in verbs[:3]:
            file.write(f"** {verb}\n\n")
            
            # Present tense drill
            file.write("*** Present Tense\n\n")
            file.write("| Subject   | Conjugation |\n")
            file.write("|-----------+------------|\n")
            for subject in ["je", "tu", "il/elle", "nous", "vous", "ils/elles"]:
                file.write(f"| {subject} | |\n")
            
            # Future tense drill
            file.write("\n*** Future Tense\n\n")
            file.write("| Subject   | Conjugation |\n")
            file.write("|-----------+------------|\n")
            for subject in ["je", "tu", "il/elle", "nous", "vous", "ils/elles"]:
                file.write(f"| {subject} | |\n")
            
            # Past tense drill
            file.write("\n*** Passé Composé\n\n")
            file.write("| Subject   | Conjugation |\n")
            file.write("|-----------+------------|\n")
            for subject in ["je", "tu", "il/elle", "nous", "vous", "ils/elles"]:
                file.write(f"| {subject} | |\n")
    
    print(f"Created drill file: {filename}")
    return filename

def generate_readme(output_dir):
    """Create README.org summarizing the drill files."""
    readme_path = os.path.join(output_dir, "README.org")
    with open(readme_path, 'w', encoding='utf-8') as file:
        file.write("#+TITLE: French Verb Drill Files\n")
        file.write("#+AUTHOR: Generated by verbiste.el\n\n")
        
        file.write("* Overview\n\n")
        file.write("These drill files are organized by topic clusters generated from semantic analysis of French verbs.\n")
        file.write("Each file contains a group of related verbs with conjugation exercises and example sentences.\n\n")
        
        file.write("* Drill Files\n\n")
        
        # List all the topic groups with descriptions
        for cluster_id, topic_info in sorted(TOPIC_LABELS.items(), key=lambda x: x[0]):
            topic_name = topic_info["name"]
            topic_desc = topic_info["description"]
            filename = f"{topic_name.lower().replace(' ', '_').replace('/', '_')}.org"
            
            file.write(f"** [[./{filename}][{topic_name}]]\n")
            file.write(f"{topic_desc}\n\n")
            file.write("Example verbs:\n")
            
            # Get representative verbs for this group
            for verb in topic_info["verbs"][:5]:
                file.write(f"- {verb}\n")
            
            file.write("\n")
    
    print(f"Created README.org in {output_dir}/")
    return readme_path

def generate_remaining_drill_files(output_dir="drills"):
    """Generate all remaining org-mode drill files based on defined topics."""
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    print(f"Generating drill files in {output_dir}/...")
    
    # Skip first three topics which were manually created
    created_files = []
    for cluster_id, topic_info in TOPIC_LABELS.items():
        # Skip the first three which we created manually
        if int(cluster_id) < 3:
            continue
            
        filename = generate_drill_file(topic_info, output_dir)
        if filename:
            created_files.append(filename)
    
    # Create README.org summarizing the drill files
    readme_path = generate_readme(output_dir)
    created_files.append(readme_path)
    
    print(f"Successfully generated {len(created_files)} files")
    
    return created_files

if __name__ == "__main__":
    # Get optional output directory
    output_dir = sys.argv[1] if len(sys.argv) > 1 else "drills"
    
    # Generate drill files
    generate_remaining_drill_files(output_dir)