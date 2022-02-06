-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='ONLY_FULL_GROUP_BY,STRICT_TRANS_TABLES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION';

-- -----------------------------------------------------
-- Schema mydb
-- -----------------------------------------------------
-- -----------------------------------------------------
-- Schema textmining
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Schema textmining
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `textmining` DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci ;
USE `textmining` ;

-- -----------------------------------------------------
-- Table `textmining`.`contrat`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `textmining`.`contrat` (
  `id_contrat` VARCHAR(10) NOT NULL,
  `type_contrat` VARCHAR(30) NULL DEFAULT NULL,
  `libelle_contrat` VARCHAR(300) NULL DEFAULT NULL,
  PRIMARY KEY (`id_contrat`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `textmining`.`experience`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `textmining`.`experience` (
  `id_experience` VARCHAR(10) NOT NULL,
  `libelle_experience` TEXT NULL DEFAULT NULL,
  `experience_exigee` VARCHAR(10) NULL DEFAULT NULL,
  PRIMARY KEY (`id_experience`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `textmining`.`poste`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `textmining`.`poste` (
  `id_poste` VARCHAR(10) NOT NULL,
  `code_rome` VARCHAR(10) NULL DEFAULT NULL,
  `libelle_rome` TEXT NULL DEFAULT NULL,
  `appellation_libelle` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`id_poste`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `textmining`.`secteur_activite`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `textmining`.`secteur_activite` (
  `id_secteur` VARCHAR(10) NOT NULL,
  `libelle_secteur` TEXT NULL DEFAULT NULL,
  `secteur_activite` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`id_secteur`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `textmining`.`offre_emploi`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `textmining`.`offre_emploi` (
  `id_offre` VARCHAR(10) NOT NULL,
  `id_contrat` VARCHAR(10) NULL DEFAULT NULL,
  `id_poste` VARCHAR(10) NULL DEFAULT NULL,
  `id_experience` VARCHAR(10) NULL DEFAULT NULL,
  `id_secteur` VARCHAR(10) NULL DEFAULT NULL,
  `categorie` VARCHAR(60) NULL DEFAULT NULL,
  `date_creation` DATETIME NULL DEFAULT NULL,
  `intitule_offre` VARCHAR(300) NULL DEFAULT NULL,
  `description_offre` TEXT NULL DEFAULT NULL,
  `salaire` VARCHAR(200) NULL DEFAULT NULL,
  `nom_entreprise` VARCHAR(200) NULL DEFAULT NULL,
  `codeCommune` VARCHAR(5) NULL DEFAULT NULL,
  PRIMARY KEY (`id_offre`),
  INDEX `id_contrat` (`id_contrat` ASC) VISIBLE,
  INDEX `id_poste` (`id_poste` ASC) VISIBLE,
  INDEX `id_experience` (`id_experience` ASC) VISIBLE,
  INDEX `id_secteur` (`id_secteur` ASC) VISIBLE,
  CONSTRAINT `offre_emploi_ibfk_1`
    FOREIGN KEY (`id_contrat`)
    REFERENCES `textmining`.`contrat` (`id_contrat`),
  CONSTRAINT `offre_emploi_ibfk_2`
    FOREIGN KEY (`id_poste`)
    REFERENCES `textmining`.`poste` (`id_poste`),
  CONSTRAINT `offre_emploi_ibfk_3`
    FOREIGN KEY (`id_experience`)
    REFERENCES `textmining`.`experience` (`id_experience`),
  CONSTRAINT `offre_emploi_ibfk_4`
    FOREIGN KEY (`id_secteur`)
    REFERENCES `textmining`.`secteur_activite` (`id_secteur`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `textmining`.`ref_regions`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `textmining`.`ref_regions` (
  `nom_region` VARCHAR(100) NULL DEFAULT NULL,
  `code_region` VARCHAR(5) NOT NULL,
  PRIMARY KEY (`code_region`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `textmining`.`ref_departement`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `textmining`.`ref_departement` (
  `nom_departement` VARCHAR(100) NULL DEFAULT NULL,
  `code_departement` VARCHAR(6) NOT NULL,
  `code_region` VARCHAR(5) NULL DEFAULT NULL,
  PRIMARY KEY (`code_departement`),
  INDEX `fk_region` (`code_region` ASC) VISIBLE,
  CONSTRAINT `ref_departement_ibfk_1`
    FOREIGN KEY (`code_region`)
    REFERENCES `textmining`.`ref_regions` (`code_region`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


-- -----------------------------------------------------
-- Table `textmining`.`ref_communes`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `textmining`.`ref_communes` (
  `id_commune` INT NOT NULL AUTO_INCREMENT,
  `nom_commune` VARCHAR(100) NULL DEFAULT NULL,
  `code_commune` VARCHAR(6) NULL DEFAULT NULL,
  `code_departement` VARCHAR(5) NULL DEFAULT NULL,
  `code_postal` VARCHAR(5) NULL DEFAULT NULL,
  PRIMARY KEY (`id_commune`),
  INDEX `fk_departements` (`code_departement` ASC) VISIBLE,
  INDEX `code_postal` (`code_postal` ASC) VISIBLE,
  INDEX `code_commune` (`code_commune` ASC) VISIBLE,
  CONSTRAINT `ref_communes_ibfk_1`
    FOREIGN KEY (`code_departement`)
    REFERENCES `textmining`.`ref_departement` (`code_departement`))
ENGINE = InnoDB
AUTO_INCREMENT = 35172
DEFAULT CHARACTER SET = utf8mb4
COLLATE = utf8mb4_0900_ai_ci;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
