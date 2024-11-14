import spacy
import sys

def example():
    nlp = spacy.load("de_GERNERMED")

    doc = nlp("Dem Patienten wurde die Einnahme von Paracetamol (500 mg, zwei Tabletten täglich, 8 Wochen lang) zur Behandlung empfohlen.")
    print(f"Satz: {doc.text}", file=sys.stdout)
    for ent in doc.ents:
        print(f"{' '*2} {ent.label_} -> {repr(ent)}", file=sys.stdout)


if __name__ == "__main__":
    example()
