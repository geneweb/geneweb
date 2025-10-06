from typing import List, Set, Dict, Tuple
import re
from name_processing import normalize_name

class ParticleManager:
    """Advanced particle management system"""

    def __init__(self):
        self.particles: Set[str] = set()
        self.language_particles: Dict[str, List[str]] = {}
        self.particle_rules: Dict[str, str] = {}

        self._init_default_particles()

    def _init_default_particles(self):
        """Initialize default particles by language - exact OCaml mapping"""

        french_particles = [
            "de", "des", "du", "de la", "de le", "d'", "de l'",
            "le", "la", "les"
        ]

        dutch_particles = [
            "van", "van de", "van der", "van den", "van het",
            "de", "den", "der", "het", "te", "ter", "ten"
        ]

        german_particles = [
            "von", "vom", "zur", "zum", "zu", "der", "den", "die", "das"
        ]

        spanish_particles = [
            "de", "del", "de la", "de los", "de las", "el", "la", "los", "las"
        ]

        italian_particles = [
            "da", "dal", "dalla", "dalle", "dallo", "dei", "del", "della",
            "delle", "dello", "di", "do", "li", "lo"
        ]

        portuguese_particles = [
            "da", "das", "de", "do", "dos"
        ]

        celtic_particles = [
            "mac", "mc", "o'", "ó", "ua"
        ]

        nordic_particles = [
            "af", "av"
        ]

        self.language_particles = {
            "french": french_particles,
            "dutch": dutch_particles,
            "german": german_particles,
            "spanish": spanish_particles,
            "italian": italian_particles,
            "portuguese": portuguese_particles,
            "celtic": celtic_particles,
            "nordic": nordic_particles
        }

        for lang_particles in self.language_particles.values():
            self.particles.update(p.lower() for p in lang_particles)

    def add_particle(self, particle: str, language: str = None):
        """Add a particle with optional language classification"""
        particle_lower = particle.lower().strip()
        self.particles.add(particle_lower)

        if language and language in self.language_particles:
            if particle not in self.language_particles[language]:
                self.language_particles[language].append(particle)

    def remove_particle(self, particle: str):
        """Remove a particle"""
        particle_lower = particle.lower().strip()
        self.particles.discard(particle_lower)

        for lang_list in self.language_particles.values():
            if particle in lang_list:
                lang_list.remove(particle)

    def is_particle(self, word: str) -> bool:
        """Check if word is a particle"""
        return word.lower().strip() in self.particles

    def find_particles_in_name(self, name: str) -> List[Tuple[str, int, int]]:
        """Find all particles in name with positions"""
        particles_found = []
        words = re.split(r'[\s\-]+', name.lower())

        current_pos = 0
        for word in words:
            word_start = name.lower().find(word, current_pos)
            if word_start != -1:
                if self.is_particle(word):
                    particles_found.append((word, word_start, word_start + len(word)))
                current_pos = word_start + len(word)

        return particles_found

    def split_name_particles(self, name: str) -> Tuple[List[str], List[str]]:
        """Split name into particles and non-particles"""
        if not name:
            return [], []

        words = re.split(r'[\s\-]+', name)
        particles_list = []
        non_particles = []

        for word in words:
            if self.is_particle(word):
                particles_list.append(word)
            else:
                non_particles.append(word)

        return particles_list, non_particles

    def format_name_with_particles(self, name: str,
                                 capitalize_particles: bool = False,
                                 particle_position: str = "keep") -> str:
        """Format name with proper particle handling"""
        if not name:
            return name

        words = re.split(r'(\s+)', name)  # Keep separators
        result_words = []

        for word in words:
            if word.isspace():
                result_words.append(word)
                continue

            word_clean = word.strip()
            if self.is_particle(word_clean):
                if capitalize_particles:
                    formatted = word_clean.title()
                else:
                    formatted = word_clean.lower()
            else:
                formatted = word_clean.capitalize() if word_clean else ""

            result_words.append(formatted)

        return ''.join(result_words)

    def sort_names_with_particles(self, names: List[str]) -> List[str]:
        """Sort names considering particles properly"""
        def sort_key(name: str) -> str:
            """Generate sort key ignoring particles"""
            particles_found, non_particles = self.split_name_particles(name)
            main_name = ' '.join(non_particles)
            return normalize_name(main_name)

        return sorted(names, key=sort_key)

    def suggest_particle_fixes(self, names: List[str]) -> List[Dict[str, any]]:
        """Suggest fixes for particle usage in names"""
        suggestions = []

        for name in names:
            particles_found = self.find_particles_in_name(name)

            if particles_found:
                for particle, start, end in particles_found:
                    original_particle = name[start:end]

                    if original_particle != original_particle.lower():
                        suggestions.append({
                            "type": "particle_capitalization",
                            "name": name,
                            "issue": f"Particle '{original_particle}' should be lowercase",
                            "suggestion": name[:start] + particle + name[end:],
                            "position": (start, end)
                        })

        return suggestions

    def load_particles_from_text(self, text: str, language: str = None):
        """Load particles from text (one per line or comma-separated)"""
        if '\n' in text:
            particles_list = text.split('\n')
        else:
            particles_list = text.split(',')

        for particle in particles_list:
            particle = particle.strip()
            if particle and not particle.startswith('#'):  # Skip comments
                self.add_particle(particle, language)

    def export_particles(self, language: str = None) -> List[str]:
        """Export particles list"""
        if language and language in self.language_particles:
            return self.language_particles[language][:]
        else:
            return sorted(list(self.particles))

    def get_particle_statistics(self, names: List[str]) -> Dict[str, any]:
        """Get statistics about particle usage"""
        stats = {
            "total_names": len(names),
            "names_with_particles": 0,
            "particle_frequency": {},
            "most_common_particles": [],
            "languages_detected": set()
        }

        for name in names:
            particles_found = self.find_particles_in_name(name)

            if particles_found:
                stats["names_with_particles"] += 1

                for particle, _, _ in particles_found:
                    stats["particle_frequency"][particle] = stats["particle_frequency"].get(particle, 0) + 1

                    for lang, lang_particles in self.language_particles.items():
                        if particle in [p.lower() for p in lang_particles]:
                            stats["languages_detected"].add(lang)

        stats["most_common_particles"] = sorted(
            stats["particle_frequency"].items(),
            key=lambda x: x[1],
            reverse=True
        )

        stats["languages_detected"] = list(stats["languages_detected"])

        return stats

_global_particle_manager = ParticleManager()

def get_particle_manager() -> ParticleManager:
    """Get global particle manager"""
    return _global_particle_manager

def is_particle_global(word: str) -> bool:
    """Check if word is particle using global manager"""
    return _global_particle_manager.is_particle(word)

def add_particle_global(particle: str, language: str = None):
    """Add particle to global manager"""
    _global_particle_manager.add_particle(particle, language)

def load_particles_file_global(filename: str):
    """Load particles from file into global manager"""
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            content = f.read()
            _global_particle_manager.load_particles_from_text(content)
    except Exception as e:
        print(f"Warning: Could not load particles file {filename}: {e}")

def get_default_particles() -> List[str]:
    """Get default particles list"""
    return _global_particle_manager.export_particles()

def set_particles_from_manager(manager: ParticleManager):
    """Set particles in main name processing from manager"""
    from name_processing import set_global_particles
    set_global_particles(manager.export_particles())

def sync_particles_with_name_processing():
    """Sync global particle manager with name processing"""
    set_particles_from_manager(_global_particle_manager)

sync_particles_with_name_processing()
