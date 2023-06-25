////////////////////////////////////////////////////////////////////////
// PigXerces
//
// Simple class whose only job is to initialize (and terminate, in the
// unlikely event that ever gets called) the Xerces C++ package.  For
// whatever reason, the Initialize and Terminate calls must be done only
// once.
//
// A reference count of init calls is kept.  If close() is actually called
// often enough, the package will be closed out.  This is unlikely though
// since most references hang around for the life of the program and thus
// their destructors never get called.
//
// A few convenience functions are also provided.
//
// This class is purely static; it should not be instantiated.
////////////////////////////////////////////////////////////////////////
#ifndef PIGXERCES_H
#define PIGXERCES_H

#include <xercesc/dom/DOM.hpp>

#ifdef XERCES_CPP_NAMESPACE_USE
XERCES_CPP_NAMESPACE_USE
#endif

class PigXerces {
  private:
    PigXerces() { }		// prevent instantiation

  protected:
    static int _initCount;
    static char _errorMessage[512];

  public:
    // Call initialize() before doing any Xerces work.  Calls are
    // reference counted so the Xerces init function is not called more
    // than once.  Calls to close() are also reference-counted and the
    // the Xerces library will be closed if the count ever goes to 0.

    static void initialize();
    static void close();

    // Convenience routine to parse an XML file.  Returns NULL on
    // error (or rather, some Xerces object that compares to NULL).
    // The error text is output via PigModelBase::printStaticMsg().

    static DOMDocument *parseFile(const char *filename);

    // Convenience routines for accessing the DOM objects.  Most of these
    // are simply wrappers that deal with transcoding for you automatically,
    // or other type conversion (e.g. Node to Element), so you only have to
    // deal with char* strings.

    // Must deaellocate returned string using XMLString::release(&str);
    // Returns empty string if not found

    static char *getAttribute(DOMElement *el, const char *attr)
	{  XMLCh *xattr = XMLString::transcode(attr);
	   const XMLCh *xresult = el->getAttribute(xattr);
	   XMLString::release(&xattr);
	   return XMLString::transcode(xresult);
	};

    // Same as above but returns a C string you can deallocate with "delete".
    // This is probably not necessary, but just being pedantic.

    static char *getAttributeCstr(DOMElement *el, const char *attr)
	{  char *res = getAttribute(el, attr);
	   char *rtn = strdup(res);
	   XMLString::release(&res);
	   return rtn;
	}

    // Returns an attribute as an int.  Returns the value passed in if not found

    static int getAttributeInt(DOMElement *el, const char *attr, int def)
	{  XMLCh *xattr = XMLString::transcode(attr);
	   const XMLCh *xresult = el->getAttribute(xattr);
	   XMLString::release(&xattr);
	   if (XMLString::stringLen(xresult) == 0)
	       return def;
	   return XMLString::parseInt(xresult);
	}

    // Of course there is no parseFloat...

    static double getAttributeDouble(DOMElement *el,const char *attr,double def)
	{  char *res = getAttribute(el, attr);
	   double rtn = def;
	   if (strlen(res) != 0)
	       rtn = atof(res);
	   XMLString::release(&res);
	   return rtn;
	}

    // Returns true if value matches the attr
    // Interesting there are no comparison functions for (XMCh, char*).

    static int attrEquals(DOMElement *el, const char *attr, const char *value)
	{  char *res = getAttribute(el, attr);
	   int match = (strcmp(res, value) == 0);
	   XMLString::release(&res);
	   return match;
	}

    // Returns true if value matches the attr, ignoring case
    // Interesting there are no comparison functions for (XMCh, char*).

    static int attrEqualsIgnoreCase(DOMElement *el, const char *attr,
							const char *value)
	{  char *res = getAttribute(el, attr);
	   int match = (strcasecmp(res, value) == 0);
	   XMLString::release(&res);
	   return match;
	}

    // Transcodes the node name for you.  Should not return NULL... I think.

    static DOMNodeList *getElementsByTagName(DOMElement *el, const char *name)
	{  XMLCh *xname = XMLString::transcode(name);
	   DOMNodeList *nodes = el->getElementsByTagName(xname);
	   XMLString::release(&xname);
	   return nodes;
	};

    // Casts result to Element for you, after type checking.

    static DOMElement *nextElement(DOMNodeList *list, int num)
	{  DOMNode *node = list->item(num);
	   if (node->getNodeType() != DOMNode::ELEMENT_NODE)
	       return NULL;
	   return (DOMElement *)node;
	};

    // Gets a single named element (the first if multiple, but ignores others)

    static DOMElement *getOneElement(DOMElement *el, const char *name)
	{  DOMNodeList *list = getElementsByTagName(el, name);
	   if (list->getLength() == 0)
	       return NULL;
	   return nextElement(list, 0);
	};

    //------------
    // Convenience methods for creating documents
    //------------

    // Creates a document

    static DOMDocument *createDocument(const char *root)
	{  XMLCh *xcore = XMLString::transcode("Core");	// whatever this means..
	   DOMImplementation *impl =
			DOMImplementationRegistry::getDOMImplementation(xcore);
	   XMLString::release(&xcore);
	   XMLCh *xroot = XMLString::transcode(root);
	   DOMDocument *doc = impl->createDocument(NULL, xroot, NULL);
	   XMLString::release(&xcore);
	   return doc;
	}

    // Transcodes the node name for you

    static DOMElement *createElement(DOMDocument *doc, const char *name)
	{  XMLCh *xname = XMLString::transcode(name);
	   DOMElement *el = doc->createElement(xname);
	   XMLString::release(&xname);
	   return el;
	};

    // Transcodes the strings for you

    static void setAttribute(DOMElement *el, const char *attr,const char *value)
	{  XMLCh *xattr = XMLString::transcode(attr);
	   XMLCh *xvalue = XMLString::transcode(value);
	   el->setAttribute(xattr, xvalue);
	   XMLString::release(&xattr);
	   XMLString::release(&xvalue);
	}

};

#endif

