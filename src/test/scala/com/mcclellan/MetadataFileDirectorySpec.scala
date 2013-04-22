package com.mcclellan

import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.ShouldMatchersForJUnit
import java.io.File

@RunWith(classOf[JUnitRunner])
class MetadataFileDirectorySpec extends FlatSpec with ShouldMatchersForJUnit {
	class ResourceTest(val testFolder : String) {
		val metadataPath = new File(this.getClass().getResource("/com/mcclellan/metadata/" + testFolder).toURI)
	}

	"A metadataFileDirectory" should "load a single file" in {
		new ResourceTest("singleFile") {
			val files = new MetadataFileDirectory(metadataPath)
			assert(files.metadataFiles.size === 1)
		}
	}

	it should "load multiple files" in {
		new ResourceTest("multiFile") {
			val files = new MetadataFileDirectory(metadataPath)
			assert(files.metadataFiles.size === 2)
			println(files.metadataFiles)
		}
	}

	it should "throw an error when missing the return file" in {
		new ResourceTest("missing") {
			intercept[AssertionError] {
				val files = new MetadataFileDirectory(metadataPath).metadataFiles
			}
		}
	}
	
	it should "throw an error when there are too many return files" in {
		new ResourceTest("missing") {
			intercept[AssertionError] {
				val files = new MetadataFileDirectory(metadataPath).metadataFiles
			}
		}
	}
}